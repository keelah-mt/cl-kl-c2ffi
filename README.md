# C2FFI Translator

A custom translator designed to produce CFFI bindings that work *differently* ™️. This tool was built to solve specific pain points encountered while binding [WebGPU](https://github.com/keelah-mt/cl-kl-wgpu) and to "address" issues like [DEFCTYPE alias for struct skips type translations](https://github.com/cffi/cffi/issues/295). And by "address," I mean "work around," obviously.

## Why this exists

The standard [c2ffi](https://github.com/cffi/cffi/tree/master/src/c2ffi) tool outputs a `defctype` for every corresponding C typedef. While this looks nice and clean, it tends to break CFFI's translation dispatch. At least, that’s my working theory. 

The original version of this project was a month-long deep dive into Common Lisp.That’s all outdated now. I spent another month teaching myself [Coalton](https://github.com/coalton-lang/coalton), which I now believe is the best foundation for a parser/translator project. Common Lisp is already a solid choice of course but Coalton makes everything extra sweet. I don’t think I’ll be looking at other languages for a long time. 🍰

The result is still a hot mess, but it is definitely more maintainable thanks to Coalton . When I return to this in a few months, I’ll have the reassurance that I won't break everything instantly. That is a massive relief given the state of my test coverage...

## How is this tool different?

This is a two-pass translator. It takes data from `c2ffi`, parses it, establishes type references, and then performs a second pass to decide whether emitting a `defctype` actually makes sense. 

The current rules are:
1. **Absolutely no `defctype` for enums.** This resolves issues where translations wouldn't kick in for enum values coming from the C side.
2. **Conditional use of `defctype` for structs.** If a struct is not opaque (i.e., it’s not a forward declaration or an empty placeholder), we skip the `defctype` alias and resolve the field directly to the actual struct.
3. **Constant Resolution.** It can resolve constant aliases across multiple levels (e.g., `special_int_t` => `normal_int_t` => `:int`). The standard translator often bails out on these chains. 😛

### Coalton Version Perks:
* **WGPU Ready:** Parses WGPU headers with no errors, and the switch from the previous CL version was seamless.
* **GLFW Progress:** I started the Coalton rewrite to address errors in GLFW bindings. I am very close to parsing the entire collection of Wayland/X11/GLFW headers without errors. While I haven't tested running GLFW with this version yet (it still sits on a subset of manual bindings), the parser is nearly there.

## More to come?

Maybe. Or maybe I’ll finally realize I was just "holding CFFI wrong" the whole time, the clouds will part, and this tool will become obsolete. Until then, I can finally get back to where I started: my WGPU project.

**Note:** This is based on my own [fork of c2ffi](https://github.com/keelah-mt/c2ffi). I decided to use the S-expression output, found it was incomplete, and had to extend it to get the data I needed.

**Note:** `clang/c2ffi` is a bit of a moving target on Arch Linux due to Clang updates. I’m currently holding back updates to avoid (possible) Clang 22+ breakage, but Clang 21 still works fine.
