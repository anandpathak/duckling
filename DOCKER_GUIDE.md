# Docker Guide for Duckling

## Quick Start (Pre-built Image)

If you just want to run Duckling without building:

```bash
# Pull the pre-built image
docker pull dafal/duckling-past:latest

# Run the container
docker run -p 8000:8000 dafal/duckling-past:latest
```

The server will be available at `http://localhost:8000`

**Note:** This uses the pre-built image from Docker Hub, which may not include your local changes.

---

## Build and Run Locally (Recommended for Development)

If you've made changes to the code (like the Indonesian time parsing improvements), you need to build the Docker image locally:

### Step 1: Build the Docker Image

```bash
# Build the image (this will take 10-30 minutes the first time)
docker build -t duckling-id:latest .

# Or with a specific tag
docker build -t duckling-id:v1.0 .
```

**What happens during build:**
- Downloads Haskell 9.2 base image
- Installs dependencies (libpcre3, build tools)
- Runs `stack setup` (downloads GHC and dependencies)
- Runs `stack install` (compiles the entire project)
- Creates a runtime image with just the binary

**Build time:** 
- First build: 20-40 minutes (downloads everything)
- Subsequent builds: 5-15 minutes (if only code changed)

### Step 2: Run the Container

```bash
# Run the container
docker run -p 8000:8000 duckling-id:latest

# Or run in detached mode (background)
docker run -d -p 8000:8000 --name duckling duckling-id:latest

# View logs
docker logs duckling

# Stop the container
docker stop duckling

# Remove the container
docker rm duckling
```

### Step 3: Test the Server

```bash
# Test with curl
curl -XPOST http://localhost:8000/parse \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'locale=id_ID&text=hari ini'

# Or with JSON
curl -XPOST http://localhost:8000/parse \
  -H "Content-Type: application/json" \
  -d '{
    "locale": "id_ID",
    "text": "hari ini",
    "dims": ["time"]
  }'

# Test Indonesian time parsing
curl -XPOST http://localhost:8000/parse \
  -H "Content-Type: application/json" \
  -d '{
    "locale": "id_ID",
    "text": "Senin jam 3",
    "dims": ["time"]
  }'
```

---

## Docker Commands Cheat Sheet

```bash
# Build
docker build -t duckling-id:latest .

# Run (foreground)
docker run -p 8000:8000 duckling-id:latest

# Run (background)
docker run -d -p 8000:8000 --name duckling duckling-id:latest

# View logs
docker logs -f duckling

# Stop
docker stop duckling

# Start (if stopped)
docker start duckling

# Remove container
docker rm duckling

# Remove image
docker rmi duckling-id:latest

# List running containers
docker ps

# List all containers (including stopped)
docker ps -a

# List images
docker images

# Execute command in running container
docker exec -it duckling /bin/bash

# View container resource usage
docker stats duckling
```

---

## Troubleshooting

### Build Fails with Out of Memory (OOM)

The Dockerfile mentions that parallel builds can cause OOM. If this happens:

1. **Option 1:** Build with single core (slower but safer)
   ```bash
   # Edit Dockerfile line 30, change to:
   RUN stack install --install-ghc -j1
   ```

2. **Option 2:** Increase Docker memory limit
   - Docker Desktop: Settings → Resources → Memory (increase to 4GB+)

### Build Takes Too Long

**Optimizations already in place:**
- BuildKit cache mounts for Stack index and GHC (persists across builds)
- Only EN and ID languages are compiled (much faster)
- Layer caching is optimized for dependency changes

**IMPORTANT: Always use BuildKit for faster builds:**

The Dockerfile uses BuildKit cache mounts which require BuildKit to be enabled:

```bash
# Build with BuildKit (REQUIRED for cache mounts to work)
DOCKER_BUILDKIT=1 docker build -t duckling-id:latest .

# Or enable BuildKit permanently:
export DOCKER_BUILDKIT=1
# Then just use: docker build -t duckling-id:latest .
```

**Build time expectations (with BuildKit):**
- First build: 20-40 minutes (downloads Hackage index ~200MB and GHC ~260MB)
- Subsequent builds (code changes only): 2-5 minutes (uses cached index and GHC)
- Subsequent builds (dependency changes): 5-10 minutes (index cached, only new deps download)

**Without BuildKit:**
- First build: 20-40 minutes
- Subsequent builds: 10-20 minutes (index re-downloads if cache invalidated)

### Port Already in Use

If port 8000 is already in use:

```bash
# Use a different port (e.g., 8001)
docker run -p 8001:8000 duckling-id:latest

# Then access at http://localhost:8001
```

### Test Indonesian Locale

Make sure to use `id_ID` or `id` as the locale:

```bash
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=hari ini&dims=["time"]'
```

---

## Development Workflow

1. **Make code changes** (e.g., in `Duckling/Time/ID/Rules.hs`)
2. **Rebuild Docker image:**
   ```bash
   docker build -t duckling-id:latest .
   ```
3. **Stop old container:**
   ```bash
   docker stop duckling
   docker rm duckling
   ```
4. **Run new container:**
   ```bash
   docker run -d -p 8000:8000 --name duckling duckling-id:latest
   ```
5. **Test your changes:**
   ```bash
   curl -XPOST http://localhost:8000/parse \
     -d 'locale=id_ID&text=YOUR_TEST_TEXT&dims=["time"]'
   ```

---

## Alternative: Run Without Docker

If you have Haskell/Stack installed locally:

```bash
# Install dependencies
stack setup

# Build
stack build

# Run
stack exec duckling-example-exe
```

This is faster for development but requires Haskell environment setup.

---

## Testing Indonesian Time Parsing

After starting the container, test with:

```bash
# Basic relative time
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=hari ini'

# Days of week
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=Senin'

# Dates
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=15 Januari 2024'

# Time
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=pukul 14:30'

# Holidays
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=hari kemerdekaan'
```

---

## Notes

- The Docker image is quite large (~1-2GB) due to Haskell dependencies
- Build time depends on your machine (CPU, RAM, disk speed)
- The server runs on port 8000 by default
- All dimensions are enabled by default, but you can specify with `dims` parameter

