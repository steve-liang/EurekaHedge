---
title: "EurekaHedge Dashboard"
output: github_document
---



## Introduction

This project uses Quandl/Nasdaq Data Link's EurekaHedge database to chart hedge fund performance across strategies.

A GitHub Actions workflow (`.github/workflows/screenshots.yml`) runs monthly (and on demand), installs the latest CRAN packages, and runs `render.R`, uploading four screenshots as a build artifact:

1. Trailing 12-month performance curve
2. Trailing 60-month performance curve
3. Latest performance ranking
4. Correlation heatmap

Set the repository secret `QUANDL_API_KEY` (locally: the `QUANDL_API_KEY` environment variable) before running.
