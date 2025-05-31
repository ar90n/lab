# %%
import textwrap
import os

import PIL.Image
import dotenv
import google.generativeai as genai

from IPython.display import display
from IPython.display import Markdown
# %% [markdown]
dotenv.load_dotenv()

def to_markdown(text):
  text = text.replace('•', '  *')
  return Markdown(textwrap.indent(text, '> ', predicate=lambda _: True))
# %%
## Configure the API key
genai.configure(api_key=os.environ['GEMINI_API_KEY'])

# %%
## List available models
for m in genai.list_models():
  if 'generateContent' in m.supported_generation_methods:
    print(m.name)
model = genai.GenerativeModel('gemini-1.5-flash')
# %%
## Generate text content
response = model.generate_content("What is the meaning of line?")
to_markdown(response.text)
# %%
## Generate image description with a system prompt
img = PIL.Image.open('image.jpg')
prompt = ["Describe the picture:", img]
response = model.generate_content(prompt)
to_markdown(response.text)
# %%
## Generate chat response
chat = model.start_chat()
response = chat.send_message("What is the meaning of line?")
to_markdown(response.text)
# %%
## Generate embedding
result = genai.embed_content(
  model="models/embedding-001",
  content="What is the meaning of life?",
  task_type="retrieval_document",
  title="Embedding of single string")
print(str(result['embedding'])[:50], '... TRIMMED]')
# %%