#!/bin/bash

HOOKS_DIR=$(git rev-parse --show-toplevel)/.git/hooks
cat > $HOOKS_DIR/post-checkout << EOF
#!/bin/bash
# Define the users who should have write privileges
WRITE_USERS=("messierkp")
 
# Get the current username
CURRENT_USER=\$(whoami)
 
# Set default permissions (read and execute)
DEFAULT_PERMISSIONS=554
 
# Set write permissions (read, write, and execute)
WRITE_PERMISSIONS=754
 
# Function to set permissions
set_permissions() {
    local permissions=\$1
    local file=\$2
    chmod \$permissions \$file
}
 
# Check if the current user is in the list of write users
if [[ " \${WRITE_USERS[@]} " =~ " \${CURRENT_USER} " ]]; then
    set_permissions \$WRITE_PERMISSIONS _targets.R
else
    set_permissions \$DEFAULT_PERMISSIONS _targets.R
fi
EOF

chmod +x $HOOKS_DIR/post-checkout

 
WRITE_USERS=("messierkp" "songi2")
# Get the current username
CURRENT_USER=\$(whoami)
 
# Set default permissions (read and execute)
DEFAULT_PERMISSIONS=554
# Set write permissions (read, write, and execute)
WRITE_PERMISSIONS=754

# Check if the current user is in the list of write users
if [[ " ${WRITE_USERS[@]} " =~ " ${CURRENT_USER} " ]]; then
    chmod $WRITE_PERMISSIONS _targets.R
else
    chmod $DEFAULT_PERMISSIONS _targets.R
fi
