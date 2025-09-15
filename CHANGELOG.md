# Changelog

All notable changes to this project will be documented in this file.

## 0.4.0.0 - 2025-09-15

[1c3277b](https://github.com/LitFill/src-todo/commit/1c3277b3fca6055f13320c5513b3b1ad315607df)...[74f1ddf](https://github.com/LitFill/src-todo/commit/74f1ddfd6ba90c8d72dc430a64671fc09d03f0d9)

### Bug Fixes

- Add newlines after error messages ([823e7ac](https://github.com/LitFill/src-todo/commit/823e7ace6579c472a3e2a15fe86413d394403548))
- Replace megaparsec with attoparsec ([2cedd6a](https://github.com/LitFill/src-todo/commit/2cedd6ac127064e41b5336ccd508bead87229b68))

### Documentation

- Add source documentations ([2fc32af](https://github.com/LitFill/src-todo/commit/2fc32af21c7d16958f787af6a1a516b64629b802))
- Update README to reflect UUID-based ID generation and add unregister command details ([74f1ddf](https://github.com/LitFill/src-todo/commit/74f1ddfd6ba90c8d72dc430a64671fc09d03f0d9))

### Features

- Displaying todos will not error anymore ([7e85160](https://github.com/LitFill/src-todo/commit/7e8516042996ae3d7ee5f5af658e2aeb40e66ec2))
- Add error handling with Control.Exception ([e69d95a](https://github.com/LitFill/src-todo/commit/e69d95a3be1586ca9e2a79a5a3391cec21b2acea))
- Add `hasId` for filtering ([c7fcac4](https://github.com/LitFill/src-todo/commit/c7fcac4354a6b7f5bd9ec5f83e0b880677da7362))
- Add dependency version bounds and other cabal fields ([a57318d](https://github.com/LitFill/src-todo/commit/a57318d386d0555fe85963302ac006756c775553))
- Implement UUID-based ID generation for TODOs and add unregister command ([369e636](https://github.com/LitFill/src-todo/commit/369e6364c66a22f90e1153fc5aacae7e96527d89))

### Refactor

- Simplify TODO parsing logic and remove unused functions ([d6c640f](https://github.com/LitFill/src-todo/commit/d6c640fc03c1c9c62ed272ea90b711496a4f87d3))

### Revert

- Revert the behavior of extractTodos ([ffc4a53](https://github.com/LitFill/src-todo/commit/ffc4a5352d0cbbd2bf6be97ca4e1053f7c14a339))

## 0.3.5.0 - 2025-09-14

[53e232f](https://github.com/LitFill/src-todo/commit/53e232f0c5d36b4d728123463322a17a9786cbf5)...[1c3277b](https://github.com/LitFill/src-todo/commit/1c3277b3fca6055f13320c5513b3b1ad315607df)

### Documentation

- Update changelogs ([b5cf3b3](https://github.com/LitFill/src-todo/commit/b5cf3b3da02ba3bf3fa40e12952542ca8c9c6527))
- Add readme and license ([acbb9d7](https://github.com/LitFill/src-todo/commit/acbb9d7c1a4348375adc31250867cfa7abdba5e0))

### Features

- Remove unused derived typeclasses ([3ba9892](https://github.com/LitFill/src-todo/commit/3ba98924108b6f6cc71aedf5e8e0f99bdedf31b3))
- Add -c|--compact option for List and Show ([1f496c5](https://github.com/LitFill/src-todo/commit/1f496c569ad170cf192f9187525e3fc090a7241b))

### Miscellaneous Tasks

- Bump version ([1c3277b](https://github.com/LitFill/src-todo/commit/1c3277b3fca6055f13320c5513b3b1ad315607df))

### Styling

- Using fully qualified names ([2fba7be](https://github.com/LitFill/src-todo/commit/2fba7be1b0da22e72d9f4f8c72d7297a923d6975))
- Using qualified names ([4940e37](https://github.com/LitFill/src-todo/commit/4940e377b908a2d31df231c4ee912741cb3680b6))

## 0.3.1.0 - 2025-09-14

[c316bc5](https://github.com/LitFill/src-todo/commit/c316bc52f75a65df843a1ea3939eab9efb4bd16d)...[53e232f](https://github.com/LitFill/src-todo/commit/53e232f0c5d36b4d728123463322a17a9786cbf5)

### Bug Fixes

- Use `orDefault` in othe sub commands ([71b63a6](https://github.com/LitFill/src-todo/commit/71b63a6c5785f5775d5faacb14446823857e4165))
- Use `many` instead of `some` ([47b57bf](https://github.com/LitFill/src-todo/commit/47b57bffa1b927eac7afe8335f023738b6019894))

### Documentation

- Update changelogs ([d592856](https://github.com/LitFill/src-todo/commit/d5928560a95c363184d1d15672248b44c7f7e8bc))

### Miscellaneous Tasks

- Bump minor version ([53e232f](https://github.com/LitFill/src-todo/commit/53e232f0c5d36b4d728123463322a17a9786cbf5))

### Styling

- Tidy the lang pragma and imports ([ae85d5d](https://github.com/LitFill/src-todo/commit/ae85d5d94ca185d732109fc9a4a969fbc1e7a032))

## 0.3.0.0 - 2025-09-14

[e914ee4](https://github.com/LitFill/src-todo/commit/e914ee411906117d3343fc73f84790e149a7972c)...[c316bc5](https://github.com/LitFill/src-todo/commit/c316bc52f75a65df843a1ea3939eab9efb4bd16d)

### Bug Fixes

- The changelog generator to use proper link ([edc4d13](https://github.com/LitFill/src-todo/commit/edc4d137c68d25c542bf2d002b7c1262878ee220))
- No longger error when no Id ([a110e1b](https://github.com/LitFill/src-todo/commit/a110e1b79c70fc5f0f6c20efb0ac03d3464e5e08))

### Features

- Imports for the next few commits ([251f07b](https://github.com/LitFill/src-todo/commit/251f07bb990e472de05e28d46183379540291457))
- Adding default "." if no file specified ([d26ba9a](https://github.com/LitFill/src-todo/commit/d26ba9a656206221c2d716a55981ef5b8de2b7c8))
- Using directory-tree to extract Todos ([d73cba0](https://github.com/LitFill/src-todo/commit/d73cba0145e1a0fba5ecf8bf4340dd510d69d2f4))

### Miscellaneous Tasks

- Bump version and update docs ([7929dc1](https://github.com/LitFill/src-todo/commit/7929dc1ad6b4f353773752717e3066aec7e7c797))
- Bump version ([c316bc5](https://github.com/LitFill/src-todo/commit/c316bc52f75a65df843a1ea3939eab9efb4bd16d))

### Refactor

- Remove sample.hs ([cf7d7a3](https://github.com/LitFill/src-todo/commit/cf7d7a3faa0e7867450fe81fc04cd06641db977f))

## 0.2.0.0 - 2025-09-13

[837c0b4](https://github.com/LitFill/src-todo/commit/837c0b40973e31cb5616eef77220289888a79420)...[e914ee4](https://github.com/LitFill/src-todo/commit/e914ee411906117d3343fc73f84790e149a7972c)

### Documentation

- Configure and update CHANGELOG.md ([0b585a0](https://github.com/LitFill/src-todo/commit/0b585a02db048a6e31d36c93cde1a1f7a3facacd))

### Features

- Using optparse-applicative now ([ed473ca](https://github.com/LitFill/src-todo/commit/ed473ca1a79fd02aefcaae56b80e8104059b5dbf))
- Imports for next few commits ([1792edd](https://github.com/LitFill/src-todo/commit/1792eddd9d28efce6fd2c2d6d5fa60e00e6819d6))
- Refactor `replaceAtLine` and add banners ([d4b6060](https://github.com/LitFill/src-todo/commit/d4b60603a7ccdb00b763953b21ea13393a4b23ba))
- Add `displayTodo` ([e914ee4](https://github.com/LitFill/src-todo/commit/e914ee411906117d3343fc73f84790e149a7972c))

### Refactor

- Refactor: style `handleCommand` and `main`, use `traverse` instead of ([7ed4adb](https://github.com/LitFill/src-todo/commit/7ed4adbb2c8f312986ebe6b6f04182a351bf6ada))

### Styling

- Using this thing (&) ([f6d6a5b](https://github.com/LitFill/src-todo/commit/f6d6a5b43cc1caca53e46cad0dd603ae4f210cf9))
- Reduce indents ([4a52ebd](https://github.com/LitFill/src-todo/commit/4a52ebd2a29f540f25da3f4a3148351aa45b71e5))

## 0.1.0.0 - 2025-09-13

### Fix

- Repair sample file and unbreak main logic ([837c0b4](https://github.com/LitFill/src-todo/commit/837c0b40973e31cb5616eef77220289888a79420))

### Init

- Feature parity with Tsoding's todo in OCaml ([a69e78f](https://github.com/LitFill/src-todo/commit/a69e78fe104f8b595ad04d88c7e46ca7f281c44c))

<!-- generated by git-cliff -->
