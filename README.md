# C2FFI Translator

A custom translator designed to produce CFFI bindings that work *differently*™️. This tool was built to solve specific pain points encountered while binding [WebGPU](https://github.com/keelah-mt/cl-kl-wgpu) and to "address" issues like [DEFCTYPE alias for struct skips type translations](https://github.com/cffi/cffi/issues/295). And by "address" I mean "work around", obviously.

## Why this exists

The standard [c2ffi](https://github.com/cffi/cffi/tree/master/src/c2ffi) tool outputs a `defctype` for every corresponding C typedef. While this looks nice and clean, it tends to break cffi's translation dispatch. At least, that’s my working theory. 

It took me a month to build this while painfully teaching myself Common Lisp on the fly, so I don't even fully remember exactly what was broken in my WGPU project when I decided I needed to write my own translator. 😂

## How is this tool different?

This is a two-pass translator. It takes data from `c2ffi`, parses it, establishes type references, and then performs a second pass to decide whether emitting a `defctype` actually makes sense. 

The current rules are:
1. **Absolutely no `defctype` for enums.** This seems to resolve issues where translations wouldn't kick in for enum values coming from the C side.
2. **Conditional use of `defctype` for structs.** If a struct is not opaque (i.e., it’s not a forward declaration or an empty placeholder), we skip the `defctype` alias and resolve the field directly to the actual struct.
3. **Constant Resolution.** It can resolve constant aliases across multiple levels, e.g. `special_int_t` => `normal_int_t` => `:int`, the standard translator bails out on this. 😛

## More to come?

Maybe. Or maybe I’ll finally realize I was just "holding cffi wrong" the whole time, the clouds will part, and this tool will become obsolete. Until then, I can finally get back to where I started - to my WGPU project.

**Note:** This is based on my own [fork of c2ffi](https://github.com/keelah-mt/c2ffi). I decided to use the S-expression output, found it was incomplete, and had to extend it a bit to get the data I needed.
