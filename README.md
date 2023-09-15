# chatgpt
> Use Martian inside Emacs

This Emacs Code extension allows you to use the Martian backend to generate
code or natural language responses from OpenAI's [ChatGPT](https://openai.com/blog/chatgpt)
right within the editor.

## Installation
For Imilienski lab members: 

```
source ~/lab/scripts/setup_deimacs 
```

### Manual Installation
Add the following lines to the `.spacemacs` configuration file:

```
dotspacemacs-additional-packages '(
  ...
    (openai :location (recipe :fetcher github :repo "emacs-openai/openai"))
    (deimacs :location (recipe :fetcher github :repo "shihabddier/deimacs"))
 )

;; at end of file
(setq openai-key "[API-KEY]")
```

Spacemacs will automatically fetch the latest version of the package and
install it.

## Usage

To start this package:

```
M-x chatgpt
```

You will then be asked to insert your response; in this window, you press
<kbd>return</kbd> to send the message, and <kbd>Shift</kbd>+<kbd>return</kbd>
to insert a newline like a normal browser!

## Variables

- `chatgpt-model` - ID of the model to use. (Default: `"gpt-3.5-turbo"`)
- `chatgpt-max-tokens` - The maximum number of tokens to generate in the completion. (Default: `2000`)
- `chatgpt-temperature` - What sampling temperature to use. (Default: `1.0`)
- `chatgpt-input-method` - Method to receive input. (Default: `'window`)
- `chatgpt-spinner-type` - Type of the spinner. (Default: `'moon`)
- `chatgpt-display-tokens-info` - Non-nil we display tokens information for each request. (Default: `t`)
- `chatgpt-animate-text` - Display text gradually instead of output it all at once. (Default: `t`)
- `chatgpt-animate-fps` - Frame per seconds to display text animation. (Default: `5`)
