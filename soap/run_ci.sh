while true; do
    clear && cabal-dev build && $@
    inotifywait -qq -e modify -r src/ test/ *.cabal
done
