# quickflash

A flashcard game to learn fractions written in Elm.

## TODO:

-   Add a message letting the user know the must complete the form to submit their answer
-   Add a message history list listing the question, the submitted answer, the actual answer, and whether it is correct
-   Clean up the UI some and make the spacing nicer so no scrolling has to be done

## Build instructions:

### Optional but highly recommended:

-   `elm-format src/ --yes`
-   `elm-analyse`

### Development:

-   `elm make src/Main.elm --optimize --output elm.js`

### Production:

-   `./optimize.sh src/Main.elm`
