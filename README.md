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

### Production (First do the development build instructions):

-   `uglifyjs elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=elm.min.js`
