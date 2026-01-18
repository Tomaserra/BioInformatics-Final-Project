# Heart Failure Mortality Prediction — Phase 2 (Black-Box Deployment)

This repository packages a **clinical risk prediction service** as two Docker containers:

* **API (Plumber)**: serves a logistic-regression model as an HTTP service.
* **UI (Shiny)**: a web interface that treats the model as a **black box** and calls the API over the Docker network.

## Project structure

```
.
├─ api/
│  ├─ Dockerfile
│  ├─ plumber.R
│  └─ model/
│     └─ model_logistic_DEATH_EVENT.rds
├─ shiny/
│  ├─ Dockerfile
│  └─ app.R
├─ docker-compose.yml
└─ README.md
```

## Requirements (local)

* Docker Desktop installed and running (Windows/macOS/Linux).
* Internet access the first time (to pull the `rocker/*` base images).

## Quick start (Docker Compose)

From the repository root (where `docker-compose.yml` is):

### 1) Build + run

```bash
docker compose up -d --build
```

### 2) Check that containers are up

```bash
docker compose ps
```

### 3) Test the API

Health:

```bash
curl http://localhost:8000/health
```

Predict:

```bash
curl -X POST http://localhost:8000/predict \
  -H "Content-Type: application/json" \
  -d '{"age":60,"ejection_fraction":40,"serum_creatinine":1.2,"serum_sodium":135}'
```

### 4) Open the Shiny UI

Open in browser:

* [http://localhost:3838](http://localhost:3838)
  If your Shiny image serves the app under a subpath, use:
* [http://localhost:3838/app](http://localhost:3838/app)

### 5) Stop

```bash
docker compose down
```

To also remove Compose-created volumes (only if you used volumes):

```bash
docker compose down -v
```

## How it works

### API (Plumber)

* Starts on **port 8000**
* Endpoints:

  * `GET /health` → `{ "status": "ok" }`
  * `POST /predict` → `{ "probability": <float> }`

**Input JSON for `/predict`** (validated server-side):

* `age`: 0–120
* `ejection_fraction`: 0–100
* `serum_creatinine`: >0–20
* `serum_sodium`: 90–200

If validation fails, the API returns HTTP **400** with `{ "error": "..." }`.

### Shiny UI

* Starts on **port 3838**
* Calls the API using:

  * Inside Docker: `http://api:8000`
  * Local (no Docker network): `http://127.0.0.1:8000`

The base URL is controlled by `API_BASE_URL`:

* In Docker Compose, it should point to `http://api:8000` (service name on the Docker network).
* When running the UI locally without Compose, set it to `http://127.0.0.1:8000`.

The UI performs local validation before sending requests and shows:

* Predicted probability (as %)
* A presentation-only “risk bucket” (Low/Moderate/High), not a clinical threshold

## Codespaces / remote environments

If using GitHub Codespaces:

1. Run:

   ```bash
   docker compose up -d --build
   ```
2. Forward ports **8000** (API) and **3838** (Shiny) from the “Ports” panel.
3. Test:

   * Open forwarded **8000** URL + `/health`
   * Open forwarded **3838** URL (or `/app` if required)

## Troubleshooting

### API build fails with “COPY model/ … not found”

Your API build context does not include `model/`. Ensure:

* The model is located at `api/model/model_logistic_DEATH_EVENT.rds`
* `docker-compose.yml` builds the API with `context: ./api` (recommended)

### “API status: UNREACHABLE” in the UI

Common causes:

* API container not running: `docker compose ps`
* Wrong base URL (local vs Docker): use `http://api:8000` inside Compose, `http://127.0.0.1:8000` locally
* Ports not exposed / blocked: check `docker-compose.yml` port mappings

### Ports already in use

If 8000 or 3838 are busy, stop conflicting services or change the port mapping in `docker-compose.yml`.

## Disk cleanup (if Docker uses too much space)

Stop and remove the project containers:

```bash
docker compose down
```

Remove also Compose volumes (project-only):

```bash
docker compose down -v
```

Global cleanup (affects all Docker projects; use only if you understand the impact):

```bash
docker system prune -a
docker system prune -a --volumes
```

Check usage:

```bash
docker system df
```

## Notes

This is a university project deployment demo. The model and UI are provided for educational purposes and are not a certified clinical decision system.
