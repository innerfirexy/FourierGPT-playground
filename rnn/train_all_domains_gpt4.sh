#!/bin/bash

# Script to train RNN classifiers for all domains (writing, xsum, peerread, pubmed, harmful)
# GPT4 version

set -e  # Exit on any error

# Configuration
DOMAINS=("writing" "xsum" "peerread" "pubmed" "harmful")
MODEL_VERSIONS=("gpt-4" "gpt-4-0125-preview" "gpt-4-1106-preview" "gpt-4-turbo-2024-04-09")

# Data directory
DATA_DIR="../data/GPT4"

# Training parameters
HIDDEN_SIZE=64
NUM_LAYERS=1
DROPOUT=0.3
BATCH_SIZE=16
LEARNING_RATE=0.001
NUM_EPOCHS=50
CV_FOLDS=5
DEVICE="cpu"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}


# Function to train a single domain
train_single_domain() {
    local domain=$1
    local model_version=$2
    local output_dir="outputs/domain_experiments/${domain}_${model_version}"
    
    print_info "Training $domain domain - $model_version"
    echo "============================================================"
    
    # Create output directory
    mkdir -p "$output_dir"
    
    # Build command
    cmd=(
        python train_domain_classifier.py
        --domain "$domain"
        --model_version "$model_version"
        --file_model_version "$model_version"
        --output_dir "$output_dir"
        --data_dir "$DATA_DIR"
        --hidden_size "$HIDDEN_SIZE"
        --num_layers "$NUM_LAYERS"
        --dropout "$DROPOUT"
        --batch_size "$BATCH_SIZE"
        --learning_rate "$LEARNING_RATE"
        --num_epochs "$NUM_EPOCHS"
        --cv_folds "$CV_FOLDS"
        --device "$DEVICE"
    )
    
    echo "Command: ${cmd[*]}"
    echo
    
    # Run training
    if "${cmd[@]}"; then
        print_success "Training completed successfully for $domain - $model_version"
        return 0
    else
        print_error "Training failed for $domain - $model_version"
        return 1
    fi
}

# Function to check if training is already completed
is_training_completed() {
    local domain=$1
    local model_version=$2
    local output_dir="outputs/domain_experiments/${domain}_${model_version}"
    
    # Check if cv_results.json exists
    if [[ -f "$output_dir/cv_results.json" ]]; then
        return 0  # Training completed
    else
        return 1  # Training not completed
    fi
}

# Main function
main() {
    print_info "Starting training for all domains (GPT4)..."
    print_info "Start time: $(date '+%Y-%m-%d %H:%M:%S')"
    
    # Calculate total configurations
    total_configs=$((${#DOMAINS[@]} * ${#MODEL_VERSIONS[@]}))
    print_info "Total configurations to train: $total_configs"
    
    # Track results
    successful=0
    failed=0
    skipped=0
    
    # Create results array
    declare -a successful_configs=()
    declare -a failed_configs=()
    
    current=0
    
    # Train each domain
    for domain in "${DOMAINS[@]}"; do
        for model_version in "${MODEL_VERSIONS[@]}"; do
            current=$((current + 1))
            
            print_info "Progress: $current/$total_configs"
            
            # Check if already completed
            if is_training_completed "$domain" "$model_version"; then
                print_warning "Training already completed for $domain - $model_version, skipping..."
                skipped=$((skipped + 1))
                continue
            fi
            
            # Train the domain
            if train_single_domain "$domain" "$model_version"; then
                successful=$((successful + 1))
                successful_configs+=("$domain - $model_version")
            else
                failed=$((failed + 1))
                failed_configs+=("$domain - $model_version")
            fi
            
            echo
        done
    done
    
    # Print summary
    echo "============================================================"
    print_info "TRAINING SUMMARY"
    echo "============================================================"
    print_info "Total configurations: $total_configs"
    print_success "Successful: $successful"
    print_error "Failed: $failed"
    print_warning "Skipped: $skipped"
    
    if [[ ${#failed_configs[@]} -gt 0 ]]; then
        echo
        print_error "Failed configurations:"
        for config in "${failed_configs[@]}"; do
            echo "  - $config"
        done
    fi
    
    if [[ ${#successful_configs[@]} -gt 0 ]]; then
        echo
        print_success "Successful configurations:"
        for config in "${successful_configs[@]}"; do
            echo "  - $config"
        done
    fi
    
    print_info "End time: $(date '+%Y-%m-%d %H:%M:%S')"
    
    # Save results summary
    summary_file="all_domains_gpt4_training_summary.json"
    cat > "$summary_file" << EOF
{
    "timestamp": "$(date -Iseconds)",
    "total_configs": $total_configs,
    "successful": $successful,
    "failed": $failed,
    "skipped": $skipped,
    "successful_configs": [$(printf '"%s"' "${successful_configs[@]}" | tr '\n' ',' | sed 's/,$//')],
    "failed_configs": [$(printf '"%s"' "${failed_configs[@]}" | tr '\n' ',' | sed 's/,$//')]
}
EOF
    
    print_info "Training summary saved to: $summary_file"
}

# Check if we're in the right directory
if [[ ! -f "train_domain_classifier.py" ]]; then
    print_error "train_domain_classifier.py not found. Please run this script from the rnn directory."
    exit 1
fi

# Check if Python is available
if ! command -v python &> /dev/null; then
    print_error "Python is not installed or not in PATH"
    exit 1
fi

# Run main function
main "$@"

