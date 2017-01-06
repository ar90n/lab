What is bin2src ?
===============
bin2src is a utility tool for converint binary file int source file.

Example
===============
```sh
% cat test.jpg | ./bin2src -l c
uint8_t data[] = { 0xff,0xd8,0xff,0xdb, ... , ,0x8,0xff,0xd9, };
```

Features
===============
* Support template file.
* Require Boost filesystem, program_options and regex.
