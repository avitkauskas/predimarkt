# ---------- BUILD STAGE ----------
FROM nixos/nix:2.19.2 AS builder

WORKDIR /build

# Enable flakes
RUN mkdir -p /root/.config/nix
RUN echo "experimental-features = nix-command flakes" >> /root/.config/nix/nix.conf

# Install tools required during configure
RUN nix profile install nixpkgs#pkg-config

# Copy only files that affect dependency resolution first
COPY flake.nix flake.lock ./

# Pre-build dependencies (cached layer)
RUN nix build .#optimized-prod-server || true

# Now copy the full project
COPY . .

# Final build
RUN nix build .#optimized-prod-server

# ---------- RUNTIME STAGE ----------
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libpq5 \
    ca-certificates \
    tzdata \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy compiled binaries
COPY --from=builder /build/result/bin /app/bin

# Default environment
ENV IHP_ENV=Production
ENV PORT=8000

EXPOSE 8000

CMD ["/app/bin/RunProdServer"]
