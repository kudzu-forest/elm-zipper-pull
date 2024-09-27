# ZipperPull
<img src='https://github.com/user-attachments/assets/3976b543-057e-41d9-be5d-385ff62b80d2' width="80%"></img>

This library makes it easier to attach an interface as a [linear zipper](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29#Example:_Bidirectional_list_traversal "list zipper") to arbitrary data types, including ordinary records. In fact, no data collection type is defined in this package: the only function exposed here is create, which returns not the data itself, but a record containing handler functions to treat your data like a list zipper. Thus, it is named ZipperPull, not Zipper itself. This idea (record-of-functions as an interface) comes from [edkelly303/elm-any-type-collections](https://package.elm-lang.org/packages/edkelly303/elm-any-type-collections/1.0.0/).

## Example
Living example in Ellie is given in [here](https://ellie-app.com/s9M3tbS6ws9a1) and [here](https://ellie-app.com/s9MWFKDFsgNa1).
