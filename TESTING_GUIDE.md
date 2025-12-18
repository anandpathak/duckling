# Testing Guide

This guide explains how to run tests for the Duckling project and when to run them.

## Types of Tests

There are **two types of tests** in this project:

1. **Haskell Unit Tests** - Fast, comprehensive tests that run without Docker
2. **API Integration Tests** - End-to-end tests that require Docker and test the full API

---

## 1. Haskell Unit Tests

These tests validate the parsing logic directly in Haskell, including:
- All corpus examples (positive and negative)
- Date range parsing
- Time expressions
- Date + time combinations
- And more...

### How to Run

#### Using Cabal (Recommended)

```bash
# Run all tests
cabal test

# Run tests with verbose output
cabal test --test-show-details=direct

# Run only Indonesian Time tests (if supported by your cabal version)
cabal test --test-option="--pattern=ID"
```

#### Using Stack

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments="--verbose"
```

### When to Run

✅ **Run these tests:**
- **Before committing** - Catch regressions early
- **After modifying rules** - Ensure your changes work correctly
- **After modifying corpus** - Verify examples still pass
- **During development** - Quick feedback loop (runs in seconds)
- **In CI/CD** - Automated validation

### What Gets Tested

- All examples in `Duckling/Time/ID/Corpus.hs`
- Negative corpus (things that should NOT match)
- Specific test cases in `tests/Duckling/Time/ID/Tests.hs`:
  - Date ranges ("7 hari terakhir")
  - Time expressions ("pukul 14:30")
  - Date + time combinations ("besok pagi")
  - Relative dates with numbers ("2 hari lalu")
  - Specific date formats ("25/12/2024")
  - Interval expressions ("dari X sampai Y")
  - Duration expressions ("dalam 2 jam")
  - Period expressions ("minggu ini", "bulan ini")

---

## 2. API Integration Tests

These tests validate the full API server through HTTP requests, testing:
- Real-world API responses
- JSON output format
- Timezone handling
- Server behavior

### Prerequisites

- Docker installed and running
- `jq` installed (optional, for better output): `brew install jq` or `sudo apt-get install jq`

### How to Run

#### Step 1: Build Docker Image

```bash
DOCKER_BUILDKIT=1 docker build . -t duckling
```

#### Step 2: Run Docker Container

```bash
# Run in foreground (Ctrl+C to stop)
docker run -p 8000:8000 duckling

# OR run in background
docker run -d -p 8000:8000 --name duckling duckling
```

#### Step 3: Run Integration Tests

```bash
./test_indonesian.sh
```

This will test:
- Relative dates (hari ini, kemarin, besok, etc.)
- Date ranges (7 hari terakhir, dalam 7 hari terakhir, etc.)
- Time expressions (pukul 14:30, jam 2 pagi, etc.)
- Date + time combinations
- Duration expressions
- Holidays
- And more...

### When to Run

✅ **Run these tests:**
- **Before releasing** - Final validation of API behavior
- **After Docker image changes** - Verify the built image works
- **When debugging API issues** - Test actual HTTP responses
- **Before deployment** - Ensure production-ready behavior

⚠️ **Note:** These tests are slower (require Docker build + server startup) and test the full stack.

### Manual API Testing

You can also test individual expressions manually:

```bash
# Parse a single expression
curl -XPOST http://localhost:8000/parse \
  --data-urlencode "locale=id_ID" \
  --data-urlencode "text=7 hari terakhir"

# Parse with reference time
curl -XPOST http://localhost:8000/parse \
  --data-urlencode "locale=id_ID" \
  --data-urlencode "text=besok jam 3 sore" \
  --data-urlencode "reftime=2025-12-18T10:00:00"
```

---

## Recommended Workflow

### During Development

1. **Make changes** to rules or corpus
2. **Run Haskell unit tests** (`cabal test`) - Fast feedback
3. **Fix any failures** - Iterate quickly
4. **Run again** until all tests pass

### Before Committing

1. **Run Haskell unit tests** (`cabal test`) - Must pass
2. **Commit your changes**

### Before Releasing/Deploying

1. **Run Haskell unit tests** (`cabal test`) - Must pass
2. **Build Docker image** (`DOCKER_BUILDKIT=1 docker build . -t duckling`)
3. **Run API integration tests** (`./test_indonesian.sh`) - Must pass
4. **Deploy**

---

## Troubleshooting

### Haskell Tests Fail

- Check that all corpus examples are valid
- Verify test expectations match actual behavior
- Run with verbose output: `cabal test --test-show-details=direct`

### API Tests Fail

- Ensure Docker container is running: `docker ps | grep duckling`
- Check if port 8000 is available: `lsof -i :8000`
- Restart container: `docker stop duckling && docker rm duckling && docker run -d -p 8000:8000 --name duckling duckling`
- Check container logs: `docker logs duckling`

### Port Already in Use

```bash
# Find and stop the container using port 8000
docker ps | grep duckling
docker stop <container_id>
docker rm <container_id>
```

---

## Test Coverage

The tests cover all forms from `test_indonesian.sh`:

✅ Relative dates (hari ini, kemarin, besok, etc.)  
✅ Relative dates with numbers (2 hari lagi, 3 hari lalu, etc.)  
✅ Date ranges (7 hari terakhir, dalam 7 hari terakhir, etc.)  
✅ Time expressions (pukul 14:30, jam 2 pagi, etc.)  
✅ Part of day (pagi, siang, sore, malam)  
✅ Date + time combinations (besok pagi, kemarin malam, etc.)  
✅ Specific dates (13 desember, 25/12/2024, etc.)  
✅ Days of week (senin, selasa, etc.)  
✅ Duration expressions (dalam 2 jam, 2 jam yang lalu, etc.)  
✅ Holidays (hari kemerdekaan, hari raya natal, etc.)  
✅ Interval expressions (dari X sampai Y)  
✅ Period expressions (minggu ini, bulan ini, awal bulan)  

---

## Summary

| Test Type | Command | Speed | When to Use |
|-----------|---------|-------|-------------|
| **Haskell Unit Tests** | `cabal test` | ⚡ Fast (seconds) | During development, before committing |
| **API Integration Tests** | `./test_indonesian.sh` | 🐢 Slower (minutes) | Before releasing, after Docker changes |

**Best Practice:** Run Haskell unit tests frequently during development, and API integration tests before important milestones.
