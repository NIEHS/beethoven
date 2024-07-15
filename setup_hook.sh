#!/bin/bash

HOOKS_DIR=$(git rev-parse --show-toplevel)/.git/hooks
echo '#!/bin/bash' > $HOOKS_DIR/post-checkout
echo 'chmod 740 _targets.R' >> $HOOKS_DIR/post-checkout
chmod +x $HOOKS_DIR/post-checkout
