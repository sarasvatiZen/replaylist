use actix_files::Files;
use actix_session::{Session, SessionMiddleware, storage::CookieSessionStore};
use actix_web::{App, HttpResponse, HttpServer, Responder, cookie::Key, get, post, route, web};
use dotenv::dotenv;
use reqwest;
use serde::Deserialize;
use std::env;

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

    let state = q
        .state
        .clone()
        .or_else(|| form.as_ref().and_then(|f| f.state.clone()))
        .unwrap_or_default();

    let redirect = if state.is_empty() {
        "/".to_string()
    } else if state.starts_with("from=") {
        format!("/?{}", state)
    } else if state.starts_with("state=") {
        format!("/?{}", state.trim_start_matches("state="))
    } else {
        format!("/?state={}", state)
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

#[get("/api/spotify/playlists/raw")]
async fn spotify_playlists_raw() -> impl Responder {
    let token = env::var("SPOTIFY_ACCESS_TOKEN").unwrap_or_default();
    let client = reqwest::Client::new();
    match client
        .get("https://api.spotify.com/v1/me/playlists?limit=50")
        .bearer_auth(&token)
        .send()
        .await
    {
        Ok(res) if res.status().is_success() => match res.json::<serde_json::Value>().await {
            Ok(json) => HttpResponse::Ok().json(json),
            Err(_) => HttpResponse::InternalServerError().body("JSON parse error"),
        },
        Ok(res) => {
            let err = res.text().await.unwrap_or_default();
            HttpResponse::BadRequest().body(format!("Spotify API error: {}", err))
        }
        Err(err) => HttpResponse::InternalServerError().body(format!("Request failed: {}", err)),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    let port = env::var("PORT").unwrap_or_else(|_| "8080".into());
    let bind_addr = format!("0.0.0.0:{}", port);
    let secret_key = Key::generate();

    HttpServer::new(move || {
        App::new()
            .wrap(
                SessionMiddleware::builder(CookieSessionStore::default(), secret_key.clone())
                    .cookie_secure(false)
                    .build(),
            )
            .service(login_callback)
            .service(login_status)
            .service(logout)
            .service(logout_all)
            .service(spotify_playlists_raw)
            .service(Files::new("/", "../frontend").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}
