use crate::reqwest::Client;
use actix_files::Files;
use actix_session::{Session, SessionMiddleware, storage::CookieSessionStore};
use actix_web::cookie::{Key, SameSite};
use actix_web::{App, HttpResponse, HttpServer, Responder, get, post, route, web};
use base64::{Engine as _, engine::general_purpose};
use dotenv::dotenv;
use jsonwebtoken::{Algorithm, EncodingKey, Header, encode};
use reqwest;
use serde::{Deserialize, Serialize};
use std::{
    env, fs,
    time::{SystemTime, UNIX_EPOCH},
};

#[derive(Serialize)]
pub struct PlaylistItem {
    id: String,
    name: String,
    cover: String,
    track_count: usize,
    tracks: Vec<String>,
}

#[derive(Deserialize)]
struct Cb {
    state: Option<String>,
    code: Option<String>,
}

#[derive(Deserialize)]
struct AppleUserTokenPayload {
    token: String,
}

#[post("/api/apple/usertoken")]
async fn save_user_token(
    session: Session,
    body: web::Json<AppleUserTokenPayload>,
) -> impl Responder {
    let token = body.token.clone();
    let _ = session.insert("apple_user_token", token);

    HttpResponse::Ok().finish()
}

#[derive(Serialize)]
struct AppleClaims {
    iss: String,
    iat: usize,
    exp: usize,
}

#[get("/api/login/spotify")]
async fn spotify_login() -> impl Responder {
    let client_id = env::var("SPOTIFY_CLIENT_ID").unwrap();
    let redirect_uri = env::var("SPOTIFY_REDIRECT_URI").unwrap();

    let url = format!(
        "https://accounts.spotify.com/authorize?client_id={}&response_type=code&redirect_uri={}&scope=playlist-read-private%20playlist-modify-private",
        client_id,
        urlencoding::encode(&redirect_uri)
    );

    HttpResponse::Found()
        .append_header(("Location", url))
        .finish()
}

pub async fn fetch_apple_playlists(
    dev_token: &str,
    user_token: &str,
) -> anyhow::Result<Vec<PlaylistItem>> {
    let client = Client::new();

    let playlists_resp: serde_json::Value = client
        .get("https://api.music.apple.com/v1/me/library/playlists")
        .header("Authorization", format!("Bearer {}", dev_token))
        .header("Music-User-Token", user_token)
        .send()
        .await?
        .json()
        .await?;

    let mut playlists = Vec::new();

    if let Some(items) = playlists_resp["data"].as_array() {
        for p in items {
            let id = p["id"].as_str().unwrap_or("").to_string();
            let name = p["attributes"]["name"].as_str().unwrap_or("").to_string();

            let mut cover = p["attributes"]["artwork"]["url"]
                .as_str()
                .unwrap_or("")
                .to_string();
            if !cover.is_empty() {
                cover = cover.replace("{w}x{h}", "300x300").replace("{f}", "jpg");
            }

            let href = p["relationships"]["tracks"]["href"].as_str().unwrap_or("");
            let tracks_url = if href.is_empty() {
                format!(
                    "https://api.music.apple.com/v1/me/library/playlists/{}/tracks",
                    id
                )
            } else {
                format!("https://api.music.apple.com{}", href)
            };

            let tracks_resp: serde_json::Value = client
                .get(&tracks_url)
                .header("Authorization", format!("Bearer {}", dev_token))
                .header("Music-User-Token", user_token)
                .send()
                .await?
                .json()
                .await?;

            let mut tracks = Vec::new();
            if let Some(track_items) = tracks_resp["data"].as_array() {
                for track in track_items {
                    let title = track["attributes"]["name"].as_str().unwrap_or("");
                    let artist = track["attributes"]["artistName"].as_str().unwrap_or("");
                    tracks.push(format!("{} - {}", title, artist));
                }
            }

            let track_count = p["relationships"]["tracks"]["meta"]["total"]
                .as_u64()
                .map(|x| x as usize)
                .unwrap_or(tracks.len());

            playlists.push(PlaylistItem {
                id,
                name,
                cover,
                track_count,
                tracks,
            });
        }
    }

    Ok(playlists)
}

pub async fn fetch_spotify_playlists(access_token: &str) -> anyhow::Result<Vec<PlaylistItem>> {
    let client = Client::new();

    let playlists_resp: serde_json::Value = client
        .get("https://api.spotify.com/v1/me/playlists?limit=50")
        .bearer_auth(access_token)
        .send()
        .await?
        .json()
        .await?;

    let mut playlists = Vec::new();

    if let Some(items) = playlists_resp["items"].as_array() {
        for pl in items {
            let id = pl["id"].as_str().unwrap_or("").to_string();
            let name = pl["name"].as_str().unwrap_or("").to_string();
            let cover = pl["images"][0]["url"].as_str().unwrap_or("").to_string();
            let track_count = pl["tracks"]["total"].as_u64().unwrap_or(0) as usize;

            let tracks_resp: serde_json::Value = client
                .get(format!(
                    "https://api.spotify.com/v1/playlists/{}/tracks",
                    id
                ))
                .bearer_auth(access_token)
                .send()
                .await?
                .json()
                .await?;

            let mut tracks = Vec::new();
            if let Some(items) = tracks_resp["items"].as_array() {
                for item in items {
                    let title = item["track"]["name"].as_str().unwrap_or("");
                    let artist = item["track"]["artists"][0]["name"].as_str().unwrap_or("");
                    tracks.push(format!("{} - {}", title, artist));
                }
            }

            playlists.push(PlaylistItem {
                id,
                name,
                cover,
                track_count: track_count,
                tracks,
            });
        }
    }

    Ok(playlists)
}

#[get("/api/spotify/playlists/raw")]
pub async fn spotify_raw(session: Session) -> impl Responder {
    if let Some(access_token) = session
        .get::<String>("spotify_access_token")
        .unwrap_or(None)
    {
        match fetch_spotify_playlists(&access_token).await {
            Ok(list) => HttpResponse::Ok().json(list),
            Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
        }
    } else {
        HttpResponse::Unauthorized().body("Not logged in")
    }
}

fn make_apple_dev_token() -> Result<String, String> {
    let private_key_path = env::var("APPLE_PRIVATE_KEY_PATH").map_err(|e| e.to_string())?;
    let key_id = env::var("APPLE_KEY_ID").map_err(|e| e.to_string())?;
    let team_id = env::var("APPLE_TEAM_ID").map_err(|e| e.to_string())?;
    let pem = fs::read_to_string(private_key_path).map_err(|e| e.to_string())?;

    let header = Header {
        alg: Algorithm::ES256,
        kid: Some(key_id),
        ..Default::default()
    };
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs() as usize;
    let claims = AppleClaims {
        iss: team_id,
        iat: now,
        exp: now + 60 * 60 * 24 * 180,
    };

    let key = EncodingKey::from_ec_pem(pem.as_bytes()).map_err(|e| e.to_string())?;
    encode(&header, &claims, &key).map_err(|e| e.to_string())
}

#[get("/api/apple/devtoken")]
async fn apple_devtoken() -> impl Responder {
    match make_apple_dev_token() {
        Ok(t) => HttpResponse::Ok().json(serde_json::json!({"token":t})),
        Err(e) => HttpResponse::InternalServerError().body(e),
    }
}

#[route("/api/login/{service}/callback", method = "GET", method = "POST")]
async fn login_callback(
    path: web::Path<String>,
    q: web::Query<Cb>,
    form: Option<web::Form<Cb>>,
    session: Session,
) -> impl Responder {
    let service = path.into_inner();
    let _ = session.insert(&service, true);

    if service == "spotify" {
        if let Some(code) = q
            .code
            .clone()
            .or_else(|| form.as_ref().and_then(|f| f.code.clone()))
        {
            let client_id = env::var("SPOTIFY_CLIENT_ID").unwrap();
            let client_secret = env::var("SPOTIFY_CLIENT_SECRET").unwrap();
            let redirect_uri = env::var("SPOTIFY_REDIRECT_URI").unwrap();

            let client = reqwest::Client::new();
            let res = client
                .post("https://accounts.spotify.com/api/token")
                .form(&[
                    ("grant_type", "authorization_code"),
                    ("code", code.as_str()),
                    ("redirect_uri", redirect_uri.as_str()),
                ])
                .basic_auth(client_id, Some(client_secret))
                .send()
                .await
                .unwrap();

            let json: serde_json::Value = res.json().await.unwrap();

            if let Some(acc) = json["access_token"].as_str() {
                let _ = session.insert("spotify_access_token", acc.to_string());
            }
            if let Some(rf) = json["refresh_token"].as_str() {
                let _ = session.insert("spotify_refresh_token", rf.to_string());
            }
        }
    }

    use urlencoding;

    let raw_state = q
        .state
        .clone()
        .or_else(|| form.as_ref().and_then(|f| f.state.clone()))
        .unwrap_or_default();

    let decoded = match urlencoding::decode(&raw_state) {
        Ok(cow) => cow.into_owned(),
        Err(_) => raw_state.clone(),
    };

    let mut normalized = decoded.as_str();

    if normalized.starts_with("state=") {
        normalized = &normalized["state=".len()..];
    }

    let redirect = if normalized.is_empty() {
        "/".to_string()
    } else {
        format!("/?{}", normalized)
    };

    HttpResponse::Found()
        .append_header(("Location", redirect))
        .finish()
}

#[get("/api/login/status")]
async fn login_status(session: Session) -> impl Responder {
    let apple_logged_in = session
        .get::<String>("apple_user_token")
        .unwrap_or(None)
        .is_some();
    let spotify_logged_in = session
        .get::<bool>("spotify")
        .unwrap_or(None)
        .unwrap_or(false);

    HttpResponse::Ok().json(serde_json::json!({
        "apple": apple_logged_in,
        "spotify": spotify_logged_in,
        "youtube": false,
        "amazon": false
    }))
}

#[post("/api/logout/{service}")]
async fn logout(path: web::Path<String>, session: Session) -> impl Responder {
    let service = path.into_inner();
    session.remove(&service);
    HttpResponse::Ok().json(serde_json::json!({ "message": "logout ok" }))
}

#[post("/api/logout_all")]
async fn logout_all(session: Session) -> impl Responder {
    for s in ["apple", "spotify", "youtube", "amazon"] {
        session.remove(s);
    }
    HttpResponse::Ok().json(serde_json::json!({ "message": "logout all ok" }))
}

fn make_secret_key() -> Key {
    if let Ok(b64) = env::var("SESSION_KEY_BASE64") {
        let bytes = general_purpose::STANDARD
            .decode(b64.trim())
            .expect("bad base64");

        let arr: [u8; 64] = bytes
            .try_into()
            .expect("SESSION_KEY_BASE64 must decode to 64 bytes");

        Key::from(&arr)
    } else {
        Key::generate()
    }
}

#[get("/api/apple/playlists/raw")]
async fn apple_playlists_raw(session: Session) -> impl Responder {
    let dev_token = match make_apple_dev_token() {
        Ok(t) => t,
        Err(e) => return HttpResponse::InternalServerError().body(format!("token error: {e}")),
    };

    let user_token = match session.get::<String>("apple_user_token").unwrap_or(None) {
        Some(t) => t,
        None => return HttpResponse::BadRequest().body("missing apple_user_token in session"),
    };

    let url = "https://api.music.apple.com/v1/me/library/playlists";
    let client = reqwest::Client::new();

    match client
        .get(url)
        .header("Authorization", format!("Bearer {dev_token}"))
        .header("Music-User-Token", user_token)
        .send()
        .await
    {
        Ok(res) => {
            let status = res.status();
            let body = res.text().await.unwrap_or_default();

            if status.is_success() {
                HttpResponse::Ok()
                    .content_type("application/json")
                    .body(body)
            } else {
                HttpResponse::BadRequest().body(format!("Apple API error: {body}"))
            }
        }
        Err(e) => HttpResponse::InternalServerError().body(format!("request failed: {e}")),
    }
}
#[get("/api/apple/playlists")]
async fn apple_playlists(session: Session) -> impl Responder {
    let dev_token = match make_apple_dev_token() {
        Ok(t) => t,
        Err(e) => return HttpResponse::InternalServerError().body(format!("token error: {e}")),
    };

    let user_token = match session.get::<String>("apple_user_token").unwrap_or(None) {
        Some(t) => t,
        None => return HttpResponse::BadRequest().body("missing apple_user_token in session"),
    };

    match fetch_apple_playlists(&dev_token, &user_token).await {
        Ok(list) => HttpResponse::Ok().json(list),
        Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
    }
}

#[get("/api/spotify/playlists/raw")]
async fn spotify_playlists_raw(session: Session) -> impl Responder {
    let refresh = match session
        .get::<String>("spotify_refresh_token")
        .unwrap_or(None)
    {
        Some(t) => t,
        None => return HttpResponse::BadRequest().body("no spotify refresh token"),
    };

    let client_id = env::var("SPOTIFY_CLIENT_ID").unwrap();
    let client_secret = env::var("SPOTIFY_CLIENT_SECRET").unwrap();

    let client = reqwest::Client::new();

    let token_res = client
        .post("https://accounts.spotify.com/api/token")
        .form(&[
            ("grant_type", "refresh_token"),
            ("refresh_token", refresh.as_str()),
        ])
        .basic_auth(client_id, Some(client_secret))
        .send()
        .await
        .unwrap();

    let json: serde_json::Value = token_res.json().await.unwrap();
    let access = json["access_token"].as_str().unwrap();

    let res = client
        .get("https://api.spotify.com/v1/me/playlists?limit=50")
        .bearer_auth(access)
        .send()
        .await
        .unwrap();

    let playlists: serde_json::Value = res.json().await.unwrap();

    HttpResponse::Ok().json(playlists)
}
#[get("/api/spotify/playlists")]
async fn spotify_playlists(session: Session) -> impl Responder {
    if let Some(access_token) = session
        .get::<String>("spotify_access_token")
        .unwrap_or(None)
    {
        match fetch_spotify_playlists(&access_token).await {
            Ok(list) => HttpResponse::Ok().json(list),
            Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
        }
    } else {
        HttpResponse::Unauthorized().body("not logged in")
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();

    let secret_key = make_secret_key();

    let port = env::var("PORT").unwrap_or_else(|_| "8080".into());
    let bind_addr = format!("0.0.0.0:{}", port);

    HttpServer::new(move || {
        App::new()
            .wrap(
                SessionMiddleware::builder(CookieSessionStore::default(), secret_key.clone())
                    .cookie_name("replaylist.sid".into())
                    .cookie_secure(true)
                    .cookie_same_site(SameSite::None)
                    .cookie_http_only(true)
                    .build(),
            )
            .service(spotify_login)
            .service(login_callback)
            .service(login_status)
            .service(logout)
            .service(logout_all)
            .service(apple_devtoken)
            .service(save_user_token)
            .service(apple_playlists_raw)
            .service(spotify_playlists_raw)
            .service(apple_playlists)
            .service(spotify_playlists)
            .service(Files::new("/", "../frontend").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}
