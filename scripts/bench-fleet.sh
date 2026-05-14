#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Fleet performance benchmarking script

set -euo pipefail

# Configuration
BENCH_DIR="$(cd "$(dirname "$0")/.." && pwd)"
RESULTS_DIR="${RESULTS_DIR:-$BENCH_DIR/benchmark-results}"
BASELINE_NAME="${BASELINE_NAME:-baseline}"
COMPARE_WITH="${COMPARE_WITH:-}"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# Create results directory
mkdir -p "$RESULTS_DIR"

# Run benchmarks
run_benchmarks() {
    log_info "Running Fleet benchmarks..."

    cd "$BENCH_DIR/shared-context"

    # Set performance governor if available
    if command -v cpupower &> /dev/null; then
        log_info "Setting CPU governor to performance mode..."
        sudo cpupower frequency-set -g performance || log_warn "Could not set CPU governor"
    fi

    # Run criterion benchmarks
    cargo bench --bench fleet_benchmarks -- --save-baseline "$BASELINE_NAME"

    # Reset CPU governor
    if command -v cpupower &> /dev/null; then
        sudo cpupower frequency-set -g powersave || true
    fi

    log_success "Benchmarks complete"
    log_info "Results saved to: target/criterion"
}

# Compare with baseline
compare_benchmarks() {
    if [[ -z "$COMPARE_WITH" ]]; then
        log_warn "No comparison baseline specified"
        return
    fi

    log_info "Comparing with baseline: $COMPARE_WITH"

    cd "$BENCH_DIR/shared-context"
    cargo bench --bench fleet_benchmarks -- --baseline "$COMPARE_WITH"
}

# Generate performance report
generate_report() {
    log_info "Generating performance report..."

    local report_file="$RESULTS_DIR/performance-report-$(date +%Y%m%d-%H%M%S).md"

    cat > "$report_file" <<EOF
# Fleet Performance Report

**Generated:** $(date -u +"%Y-%m-%d %H:%M:%S UTC")
**Baseline:** $BASELINE_NAME
**Platform:** $(uname -a)
**Rust Version:** $(rustc --version)

## Benchmark Results

EOF

    # Extract key metrics from criterion output
    if [[ -d "shared-context/target/criterion" ]]; then
        log_info "Processing benchmark data..."

        # List all benchmarks
        echo "### Summary" >> "$report_file"
        echo "" >> "$report_file"

        for bench_dir in shared-context/target/criterion/*/; do
            if [[ -f "$bench_dir/new/estimates.json" ]]; then
                local bench_name=$(basename "$bench_dir")
                local mean=$(jq -r '.mean.point_estimate' "$bench_dir/new/estimates.json")
                local unit=$(jq -r '.mean.unit' "$bench_dir/new/estimates.json")

                # Convert to human-readable
                printf "- **%s**: %.2f %s\n" "$bench_name" "$mean" "$unit" >> "$report_file"
            fi
        done

        echo "" >> "$report_file"
        echo "## Detailed Results" >> "$report_file"
        echo "" >> "$report_file"
        echo "See \`target/criterion/report/index.html\` for interactive visualizations." >> "$report_file"
    fi

    log_success "Report generated: $report_file"
}

# Profile with flamegraph
profile_flamegraph() {
    if ! command -v cargo-flamegraph &> /dev/null; then
        log_warn "cargo-flamegraph not found. Install with: cargo install flamegraph"
        return
    fi

    log_info "Generating flamegraph profile..."

    cd "$BENCH_DIR/shared-context"

    # Run with flamegraph
    sudo -E cargo flamegraph --bench fleet_benchmarks -- --bench --profile-time 30

    log_success "Flamegraph generated: flamegraph.svg"
}

# Memory profiling
profile_memory() {
    if ! command -v valgrind &> /dev/null; then
        log_warn "valgrind not found. Install with: sudo dnf install valgrind"
        return
    fi

    log_info "Running memory profile with valgrind..."

    cd "$BENCH_DIR/shared-context"

    # Build in release mode
    cargo build --release --bench fleet_benchmarks

    # Run with valgrind
    valgrind --tool=massif \
        --massif-out-file="$RESULTS_DIR/massif.out" \
        ./target/release/deps/fleet_benchmarks-* --bench

    # Generate report
    ms_print "$RESULTS_DIR/massif.out" > "$RESULTS_DIR/memory-profile.txt"

    log_success "Memory profile: $RESULTS_DIR/memory-profile.txt"
}

# Show usage
usage() {
    cat <<EOF
Fleet Performance Benchmarking

Usage: $0 [OPTIONS] COMMAND

Commands:
    run                 Run all benchmarks
    compare             Compare with baseline
    report              Generate performance report
    flamegraph          Generate CPU flamegraph
    memory              Profile memory usage
    all                 Run all profiling and benchmarks

Options:
    --baseline NAME     Set baseline name (default: baseline)
    --compare NAME      Compare with specific baseline
    --results DIR       Set results directory

Examples:
    # Run benchmarks and save as baseline
    $0 run

    # Run and compare with previous baseline
    $0 --compare v0.1.0 run

    # Generate all profiles
    $0 all

    # Just create flamegraph
    $0 flamegraph
EOF
}

# Main
main() {
    local command="${1:-}"

    case "$command" in
        run)
            run_benchmarks
            ;;
        compare)
            compare_benchmarks
            ;;
        report)
            generate_report
            ;;
        flamegraph)
            profile_flamegraph
            ;;
        memory)
            profile_memory
            ;;
        all)
            run_benchmarks
            generate_report
            profile_flamegraph
            profile_memory
            ;;
        ""|--help|-h)
            usage
            ;;
        *)
            echo "Unknown command: $command"
            usage
            exit 1
            ;;
    esac
}

# Parse options
while [[ $# -gt 0 ]]; do
    case "$1" in
        --baseline)
            BASELINE_NAME="$2"
            shift 2
            ;;
        --compare)
            COMPARE_WITH="$2"
            shift 2
            ;;
        --results)
            RESULTS_DIR="$2"
            shift 2
            ;;
        *)
            main "$@"
            exit 0
            ;;
    esac
done

usage
