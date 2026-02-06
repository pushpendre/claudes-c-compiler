FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# System dependencies: build tools, cross-compilers, QEMU for testing
RUN apt-get update && apt-get install -y \
    curl wget git build-essential pkg-config \
    gcc-aarch64-linux-gnu g++-aarch64-linux-gnu \
    gcc-riscv64-linux-gnu g++-riscv64-linux-gnu \
    gcc-i686-linux-gnu g++-i686-linux-gnu \
    qemu-user-static \
    nodejs npm \
    && rm -rf /var/lib/apt/lists/*

# Install Rust (stable)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

# Install Claude Code CLI
RUN npm install -g @anthropic-ai/claude-code

# Git identity (all agents commit as Claude)
RUN git config --global user.name "Claude Opus 4.6" \
    && git config --global user.email "noreply@anthropic.com"

# Merge strategy for pull --rebase
RUN git config --global pull.rebase true \
    && git config --global rebase.autoStash true

# Copy entrypoint
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

WORKDIR /workspace

ENTRYPOINT ["/entrypoint.sh"]
