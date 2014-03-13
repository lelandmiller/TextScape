# stdlib.ts

This file contains the standard textscape library written in textscape.

First create a namespace to hold items loaded in the standard library:

```ts
(makeNamespace /stdlib/)
```

## Load README

```ts
(let /stdlib.readme/ /Welcome to TextScape!
TODO:
- Pretty up interface, including theme, ace mode, tree whole line, up for history.
- Empty buffer should clear editor
- Remove console references
/)
```

## Helper Functions

```ts
(let /loadPandoc/ /(eval //pandoc// (openFile @0))/)
```

## Buffer Functions

First we initialize the buffer system, which requires us to create a namespace to hold the buffers.

```ts
(makeNamespace /Buffer/)
(makeNamespace /Buffer._System/)
```

Buffer.new! takes a name on @0 and creates a buffer

```ts
(let /Buffer.new!/ /(Buffer._System.new (cat //Buffer.// @0))/)
```

This is how we create a new buffer.

```ts
(let /Buffer._System.new/ ##
  (makeNamespace @0)
  (let (cat @0 /.content/) //)
  (let (cat @0 /.filename/) //)
  (let (cat @0 /.open!/)
       (cat /(let /// @0 /.content// (openFile @0))/
       	    /(let /// @0 /.filename// (cat (pwd) //////// @0))/))
  (let (cat @0 /.save!/)
       (cat /(writeFile / @0 /.filename / @0 /.content)/))
##)
```

(let /showBuffer/ /(cat *(cat //stdlib.Buffers.// @0))/)

(let /newBuffer/ /(let (cat //stdlib.Buffers.// @0) ////)/)

(let /putBuffer/ /(let (cat //stdlib.Buffers.// @0) @1)/)

(let /appendBuffer/ /(putBuffer @0 (cat (showBuffer @0) @1))/) 


## Finish

```ts
(let /x/ ##
This is the end.
#How does that sound?
##)

(let /stdlib.ts/ (openFile /stdlib.ts/))

(let /sayHello!/ #
  (cat /Hello, / @0 /!/)
#)
```
