use actix_files::Files;
use actix_session::{storage::CookieSessionStore, Session, SessionMiddleware};
use actix_web::cookie::{Key, SameSite};
use actix_web::{get, post, route, web, App, HttpResponse, HttpServer, Responder};
use base64::{engine::general_purpose, Engine as _};
use dotenv::dotenv;
use jsonwebtoken::{encode, Algorithm, EncodingKey, Header};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::{
    env, fs,
    time::{SystemTime, UNIX_EPOCH},
};

#[derive(Deserialize)]
struct DonatePayload {
    amount: i64,
    currency: String,
}

#[post("/api/donate")]
async fn donate(body: web::Json<DonatePayload>) -> impl Responder {
    let secret = std::env::var("STRIPE_SECRET_KEY").unwrap();

    let params = [
        ("mode", "payment"),
        ("success_url", "https://replaylist.online"),
        ("cancel_url", "https://replaylist.online"),
        (
            "line_items[0][price_data][currency]",
            body.currency.as_str(),
        ),
        ("line_items[0][price_data][product_data][name]", "Donation"),
        (
            "line_items[0][price_data][unit_amount]",
            &body.amount.to_string(),
        ),
        ("line_items[0][quantity]", "1"),
    ];

    let client = reqwest::Client::new();

    let resp = client
        .post("https://api.stripe.com/v1/checkout/sessions")
        .basic_auth(secret, Some(""))
        .form(&params)
        .send()
        .await;

    match resp {
        Ok(r) => {
            let json: serde_json::Value = r.json().await.unwrap();
            let url = json["url"].as_str().unwrap_or("").to_string();
            HttpResponse::Ok().json(serde_json::json!({ "url": url }))
        }
        Err(e) => HttpResponse::InternalServerError().body(format!("stripe error: {}", e)),
    }
}

#[derive(Deserialize)]
struct TransferPayload {
    playlist: PlaylistItem,
}

#[post("/api/transfer/to/youtube")]
async fn transfer_to_youtube(
    session: Session,
    payload: web::Json<TransferPayload>,
) -> impl Responder {
    match create_playlist_to_youtube(&session, &payload.playlist).await {
        Ok(_) => HttpResponse::Ok().body("ok"),
        Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
    }
}

#[post("/api/transfer/to/spotify")]
async fn transfer_to_spotify(
    session: Session,
    payload: web::Json<TransferPayload>,
) -> impl Responder {
    match create_playlist_to_spotify(&session, &payload.playlist).await {
        Ok(_) => HttpResponse::Ok().body("ok"),
        Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
    }
}

#[post("/api/transfer/to/apple")]
async fn transfer_to_apple(
    session: Session,
    payload: web::Json<TransferPayload>,
) -> impl Responder {
    match create_playlist_to_apple(&session, &payload.playlist).await {
        Ok(_) => HttpResponse::Ok().body("ok"),
        Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
    }
}

pub async fn create_playlist_to_youtube(
    session: &Session,
    playlist: &PlaylistItem,
) -> anyhow::Result<()> {
    let access_token = session
        .get::<String>("youtube_access_token")?
        .ok_or_else(|| anyhow::anyhow!("no youtube_access_token"))?;

    let client = reqwest::Client::new();

    let create_res: serde_json::Value = client
        .post("https://www.googleapis.com/youtube/v3/playlists?part=snippet,status")
        .bearer_auth(&access_token)
        .json(&serde_json::json!({
            "snippet": {"title": playlist.name},
            "status": {"privacyStatus": "private"}
        }))
        .send()
        .await?
        .json()
        .await?;

    let playlist_id = create_res["id"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("failed to get playlist id"))?;

    for track in &playlist.tracks {
        let query = format!("{} {}", track.title, track.artist);
        let search: serde_json::Value = client
            .get("https://www.googleapis.com/youtube/v3/search")
            .bearer_auth(&access_token)
            .query(&[
                ("part", "snippet"),
                ("type", "video"),
                ("maxResults", "1"),
                ("q", &query),
            ])
            .send()
            .await?
            .json()
            .await?;

        if let Some(video_id) = search["items"]
            .as_array()
            .and_then(|arr| arr.first())
            .and_then(|v| v["id"]["videoId"].as_str())
        {
            client
                .post("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet")
                .bearer_auth(&access_token)
                .json(&serde_json::json!({
                    "snippet": {
                        "playlistId": playlist_id,
                        "resourceId": {
                            "kind": "youtube#video",
                            "videoId": video_id
                        }
                    }
                }))
                .send()
                .await?;
        }
    }
    Ok(())
}

pub async fn create_playlist_to_apple(
    session: &Session,
    playlist: &PlaylistItem,
) -> anyhow::Result<()> {
    let dev_token = make_apple_dev_token().map_err(anyhow::Error::msg)?;
    let user_token = session
        .get::<String>("apple_user_token")?
        .ok_or_else(|| anyhow::anyhow!("no apple_user_token in session"))?;

    let client = reqwest::Client::builder().gzip(true).build()?;

    let resp = client
        .post("https://api.music.apple.com/v1/me/library/playlists")
        .header("Authorization", format!("Bearer {}", dev_token))
        .header("Music-User-Token", &user_token)
        .json(&serde_json::json!({ "attributes": { "name": playlist.name } }))
        .send()
        .await?;

    let status = resp.status();
    let body = resp.text().await?;

    if !status.is_success() {
        anyhow::bail!("create playlist failed: {}", body);
    }

    let v: serde_json::Value = serde_json::from_str(&body)?;
    let playlist_id = v["data"][0]["id"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("failed to extract playlist id"))?
        .to_string();

    for track in &playlist.tracks {
        let catalog_id = if let Some(isrc) = &track.isrc {
            let v = client
                .get("https://api.music.apple.com/v1/catalog/jp/songs")
                .header("Authorization", format!("Bearer {}", dev_token))
                .query(&[("filter[isrc]", isrc)])
                .send()
                .await?
                .json::<serde_json::Value>()
                .await?;

            v["data"]
                .as_array()
                .and_then(|arr| arr.first())
                .and_then(|song| song["id"].as_str())
                .map(|s| s.to_string())
        } else {
            let q = format!("{} {}", track.title, track.artist);
            let v = client
                .get("https://api.music.apple.com/v1/catalog/jp/search")
                .header("Authorization", format!("Bearer {}", dev_token))
                .query(&[("term", q.as_str()), ("types", "songs"), ("limit", "1")])
                .send()
                .await?
                .json::<serde_json::Value>()
                .await?;

            v["results"]["songs"]["data"]
                .as_array()
                .and_then(|arr| arr.first())
                .and_then(|s| s["id"].as_str())
                .map(|s| s.to_string())
        };

        let Some(catalog_id) = catalog_id else {
            continue;
        };

        client
            .post(format!(
                "https://api.music.apple.com/v1/me/library/playlists/{}/tracks",
                playlist_id
            ))
            .header("Authorization", format!("Bearer {}", dev_token))
            .header("Music-User-Token", &user_token)
            .json(&serde_json::json!({
                "data": [{ "id": catalog_id, "type": "catalog-songs" }]
            }))
            .send()
            .await?;
    }
    Ok(())
}

#[post("/api/apple/save_token")]
async fn save_apple_user_token(session: Session, body: String) -> impl Responder {
    session.insert("apple_user_token", body).unwrap();
    HttpResponse::Ok()
}

pub async fn create_playlist_to_spotify(
    session: &Session,
    playlist: &PlaylistItem,
) -> anyhow::Result<()> {
    let refresh = session
        .get::<String>("spotify_refresh_token")?
        .ok_or_else(|| anyhow::anyhow!("no spotify_refresh_token"))?;

    let client_id = env::var("SPOTIFY_CLIENT_ID")?;
    let client_secret = env::var("SPOTIFY_CLIENT_SECRET")?;

    let client = reqwest::Client::new();

    let token_res = client
        .post("https://accounts.spotify.com/api/token")
        .form(&[
            ("grant_type", "refresh_token"),
            ("refresh_token", refresh.as_str()),
        ])
        .basic_auth(client_id, Some(client_secret))
        .send()
        .await?;

    let json: serde_json::Value = token_res.json().await?;
    let access = json["access_token"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("no access token"))?;

    let me: serde_json::Value = client
        .get("https://api.spotify.com/v1/me")
        .bearer_auth(access)
        .send()
        .await?
        .json()
        .await?;
    let user_id = me["id"].as_str().unwrap();

    let create_res: serde_json::Value = client
        .post(format!(
            "https://api.spotify.com/v1/users/{}/playlists",
            user_id
        ))
        .bearer_auth(access)
        .json(&serde_json::json!({
            "name": playlist.name,
            "public": false
        }))
        .send()
        .await?
        .json()
        .await?;

    let new_playlist_id = create_res["id"].as_str().unwrap();

    for track in &playlist.tracks {
        let uri = if let Some(ref isrc) = track.isrc {
            //ISRC検索
            let q = format!("isrc:{}", isrc);

            let search: serde_json::Value = client
                .get("https://api.spotify.com/v1/search")
                .query(&[("q", q.as_str()), ("type", "track"), ("limit", "1")])
                .bearer_auth(access)
                .send()
                .await?
                .json()
                .await?;

            search["tracks"]["items"]
                .as_array()
                .and_then(|items| items.get(0))
                .and_then(|item| item["uri"].as_str())
                .map(|s| s.to_string())
        } else {
            //タイトル+アーティスト検索
            let query = format!("track:\"{}\" artist:\"{}\"", track.title, track.artist);

            let search: serde_json::Value = client
                .get("https://api.spotify.com/v1/search")
                .query(&[
                    ("q", query),
                    ("type", "track".into()),
                    ("limit", "1".into()),
                ])
                .bearer_auth(access)
                .send()
                .await?
                .json()
                .await?;

            search["tracks"]["items"]
                .as_array()
                .and_then(|items| items.get(0))
                .and_then(|item| item["uri"].as_str())
                .map(|s| s.to_string())
        };

        if let Some(uri) = uri {
            client
                .post(format!(
                    "https://api.spotify.com/v1/playlists/{}/tracks",
                    new_playlist_id
                ))
                .bearer_auth(access)
                .json(&serde_json::json!({ "uris": [uri] }))
                .send()
                .await?;
        }
    }
    Ok(())
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Track {
    pub title: String,
    pub artist: String,
    pub isrc: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PlaylistItem {
    pub id: String,
    pub name: String,
    pub cover: String,
    pub track_count: usize,
    pub tracks: Vec<Track>,
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
        "https://accounts.spotify.com/authorize?client_id={}&response_type=code&redirect_uri={}&scope=playlist-read-private%20playlist-modify-private%20playlist-modify-public",
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
                    let isrc = track["attributes"]["isrc"].as_str().map(|s| s.to_string());

                    tracks.push(Track {
                        title: title.to_string(),
                        artist: artist.to_string(),
                        isrc,
                    });
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
                    let isrc = item["track"]["external_ids"]["isrc"]
                        .as_str()
                        .map(|s| s.to_string());

                    tracks.push(Track {
                        title: title.to_string(),
                        artist: artist.to_string(),
                        isrc,
                    });
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

pub async fn fetch_youtube_playlists(access_token: &str) -> anyhow::Result<Vec<PlaylistItem>> {
    let client = Client::new();

    let playlists_resp: serde_json::Value = client
        .get("https://www.googleapis.com/youtube/v3/playlists")
        .query(&[("part", "snippet"), ("mine", "true"), ("maxResults", "50")])
        .bearer_auth(access_token)
        .send()
        .await?
        .json()
        .await?;

    let mut playlists = Vec::new();

    if let Some(items) = playlists_resp["items"].as_array() {
        for pl in items {
            let id = pl["id"].as_str().unwrap_or("").to_string();
            let name = pl["snippet"]["title"].as_str().unwrap_or("").to_string();
            let cover = pl["snippet"]["thumbnails"]["medium"]["url"]
                .as_str()
                .unwrap_or("")
                .to_string();

            let tracks_resp: serde_json::Value = client
                .get("https://www.googleapis.com/youtube/v3/playlistItems")
                .query(&[
                    ("part", "snippet"),
                    ("playlistId", id.as_str()),
                    ("maxResults", "50"),
                ])
                .bearer_auth(access_token)
                .send()
                .await?
                .json()
                .await?;

            let mut tracks = Vec::new();
            if let Some(video_items) = tracks_resp["items"].as_array() {
                for item in video_items {
                    let title = item["snippet"]["title"].as_str().unwrap_or("");
                    let mut artist = item["snippet"]["videoOwnerChannelTitle"]
                        .as_str()
                        .unwrap_or("")
                        .to_string();

                    //なんか公式にはTopicって表示されるらしいから消す
                    if artist.ends_with(" - Topic") {
                        artist = artist.trim_end_matches(" - Topic").to_string();
                    }

                    tracks.push(Track {
                        title: title.to_string(),
                        artist: artist.to_string(),
                        isrc: None,
                    });
                }
            }

            playlists.push(PlaylistItem {
                id,
                name,
                cover,
                track_count: tracks.len(),
                tracks,
            });
        }
    }
    Ok(playlists)
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

    let code_opt = q
        .code
        .clone()
        .or_else(|| form.as_ref().and_then(|f| f.code.clone()));

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
    } else if service == "youtube" {
        if let Some(code) = code_opt {
            let client_id = env::var("GOOGLE_CLIENT_ID").unwrap();
            let client_secret = env::var("GOOGLE_CLIENT_SECRET").unwrap();
            let redirect_uri = env::var("GOOGLE_REDIRECT_URI").unwrap();

            let client = reqwest::Client::new();
            let res = client
                .post("https://oauth2.googleapis.com/token")
                .form(&[
                    ("grant_type", "authorization_code"),
                    ("code", code.as_str()),
                    ("redirect_uri", redirect_uri.as_str()),
                    ("client_id", client_id.as_str()),
                    ("client_secret", client_secret.as_str()),
                ])
                .send()
                .await
                .unwrap();
            let json: serde_json::Value = res.json().await.unwrap();

            if let Some(acc) = json["access_token"].as_str() {
                let _ = session.insert("youtube_access_token", acc.to_string());
            }
            if let Some(rf) = json["refresh_token"].as_str() {
                let _ = session.insert("youtube_refresh_token", rf.to_string());
            }
            let _ = session.insert("youtube", true);
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
        .get::<String>("spotify_access_token")
        .unwrap_or(None)
        .is_some();
    let youtube_logged_in = session
        .get::<bool>("youtube")
        .unwrap_or(None)
        .unwrap_or(false);

    HttpResponse::Ok().json(serde_json::json!({
        "apple": apple_logged_in,
        "spotify": spotify_logged_in,
        "youtube": youtube_logged_in,
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
    for key in [
        "apple_user_token",
        "spotify_access_token",
        "spotify_refresh_token",
        "youtube_access_token",
        "youtube_refresh_token",
        "apple",
        "spotify",
        "youtube",
        "amazon",
    ] {
        session.remove(key);
    }

    HttpResponse::Ok().json(serde_json::json!({
        "message": "logout all ok"
    }))
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

#[get("/api/youtube/playlists/raw")]
async fn youtube_playlists_raw(session: Session) -> impl Responder {
    let refresh = match session
        .get::<String>("youtube_refresh_token")
        .unwrap_or(None)
    {
        Some(t) => t,
        None => return HttpResponse::BadRequest().body("no youtube refresh token"),
    };

    let client_id = env::var("GOOGLE_CLIENT_ID").unwrap();
    let client_secret = env::var("GOOGLE_CLIENT_SECRET").unwrap();
    let redirect_uri = env::var("GOOGLE_REDIRECT_URI").unwrap();

    let client = reqwest::Client::new();

    let token_res = client
        .post("https://oauth2.googleapis.com/token")
        .form(&[
            ("grant_type", "refresh_token"),
            ("refresh_token", refresh.as_str()),
            ("client_id", client_id.as_str()),
            ("client_secret", client_secret.as_str()),
            ("redirect_uri", redirect_uri.as_str()),
        ])
        .send()
        .await
        .unwrap();

    let json: serde_json::Value = token_res.json().await.unwrap();
    let access = json["access_token"].as_str().unwrap();

    let playlists = client
        .get("https://www.googleapis.com/youtube/v3/playlists")
        .query(&[("part", "snippet"), ("mine", "true"), ("maxResults", "50")])
        .bearer_auth(access)
        .send()
        .await
        .unwrap()
        .json::<serde_json::Value>()
        .await
        .unwrap();

    HttpResponse::Ok().json(playlists)
}

#[get("/api/youtube/playlists")]
async fn youtube_playlists(session: Session) -> impl Responder {
    if let Some(access_token) = session
        .get::<String>("youtube_access_token")
        .unwrap_or(None)
    {
        match fetch_youtube_playlists(&access_token).await {
            Ok(list) => HttpResponse::Ok().json(list),
            Err(e) => HttpResponse::InternalServerError().body(e.to_string()),
        }
    } else {
        HttpResponse::Unauthorized().body("not logged in")
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

#[get("/api/login/youtube")]
async fn youtube_login() -> impl Responder {
    let client_id = env::var("GOOGLE_CLIENT_ID").unwrap();
    let redirect_uri = env::var("GOOGLE_REDIRECT_URI").unwrap();

    let url = format!(
        "https://accounts.google.com/o/oauth2/v2/auth?response_type=code\
         &client_id={}&redirect_uri={}\
         &scope={}\
         &access_type=offline&include_granted_scopes=true&prompt=consent",
        urlencoding::encode(&client_id),
        urlencoding::encode(&redirect_uri),
        urlencoding::encode("https://www.googleapis.com/auth/youtube.force-ssl")
    );

    HttpResponse::Found()
        .append_header(("Location", url))
        .finish()
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
            .service(youtube_login)
            .service(login_callback)
            .service(login_status)
            .service(logout)
            .service(logout_all)
            .service(apple_devtoken)
            .service(save_user_token)
            .service(apple_playlists_raw)
            .service(spotify_playlists_raw)
            .service(youtube_playlists_raw)
            .service(apple_playlists)
            .service(spotify_playlists)
            .service(youtube_playlists)
            .service(transfer_to_spotify)
            .service(transfer_to_apple)
            .service(transfer_to_youtube)
            .service(save_apple_user_token)
            .service(donate)
            .service(Files::new("/", "../frontend").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}
