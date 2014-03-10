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
/)
```

## Helper Functions

```ts
(let /loadPandoc/ /(eval //pandoc// (openFile @0))/)
```

## Buffer Functions

First we initialize the buffer system, which requires us to create a namespace to hold the buffers.

```ts
(makeNamespace /stdlib.Buffers/)
```

Now define several basic functions for accessing buffers.

```ts

(let /showBuffer/ /(cat *(cat //stdlib.Buffers.// @0))/)

(let /newBuffer/ /(let (cat //stdlib.Buffers.// @0) ////)/)

(let /putBuffer/ /(let (cat //stdlib.Buffers.// @0) @1)/)

(let /appendBuffer/ /(putBuffer @0 (cat (showBuffer @0) @1))/) 

```

