# ZipperPull
![data_structure](https://github.com/user-attachments/assets/3976b543-057e-41d9-be5d-385ff62b80d2)

 This library makes it easier to attach interface as [linear zipper](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29#Example:_Bidirectional_list_traversal "list zipper") on arbitrary data types including ordinary records.
 Actually, no data correction type is defined in this package: the only functions exposed here is `create`, which returns  not data itself, but a record containing handler functions to treat your data like list zipper. So it is named `ZipperPull`, not Zipper itself.
 This idea (record-of-functions as an interface) is from [edkelly303/elm-any-type-collections](https://package.elm-lang.org/packages/edkelly303/elm-any-type-collections/1.0.0/).

## Example
Living example in Ellie is given in [here](https://ellie-app.com/s9M3tbS6ws9a1) and [here](https://ellie-app.com/s9MWFKDFsgNa1).
