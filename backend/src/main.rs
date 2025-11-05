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
    let _ = session.insert("apple", true);

    HttpResponse::Ok().finish()
}

#[derive(Serialize)]
struct AppleClaims {
    iss: String,
    iat: usize,
    exp: usize,
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

// プレイリスト一覧表示するやつはここ
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

#[get("/api/spotify/playlists/raw")]
async fn spotify_playlists_raw() -> impl Responder {
    let token = match env::var("SPOTIFY_ACCESS_TOKEN") {
        Ok(t) => t,
        Err(_) => return HttpResponse::BadRequest().body("missing SPOTIFY_ACCESS_TOKEN"),
    };

    let client = reqwest::Client::new();
    match client
        .get("https://api.spotify.com/v1/me/playlists?limit=50")
        .bearer_auth(&token)
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
                HttpResponse::BadRequest().body(format!("Spotify API error: {body}"))
            }
        }

        Err(e) => HttpResponse::InternalServerError().body(format!("request failed: {e}")),
    }
}

// プレイリスト一覧表示するやつはここまで

#[derive(Deserialize)]
struct Cb {
    state: Option<String>,
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

    let mut normalized: &str = decoded.as_str();
    if let Some(rest) = normalized.strip_prefix("state=") {
        normalized = rest;
    }
    if let Some(rest) = normalized.strip_prefix('?') {
        normalized = rest;
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
    let services = ["apple", "spotify", "youtube", "amazon"];
    let mut statuses = serde_json::Map::new();
    for s in services {
        let v = session.get::<bool>(s).unwrap_or(None).unwrap_or(false);
        statuses.insert(s.to_string(), serde_json::json!(v));
    }
    HttpResponse::Ok().json(statuses)
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
            .service(login_callback)
            .service(login_status)
            .service(logout)
            .service(logout_all)
            .service(spotify_playlists_raw)
            .service(apple_devtoken)
            .service(save_user_token)
            .service(apple_playlists_raw)
            .service(save_user_token)
            .service(Files::new("/", "../frontend").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}
