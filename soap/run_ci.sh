while true; do
    clear && cabal build && $@
    inotifywait -qq -e modify -r src/ test/ *.cabal
done
