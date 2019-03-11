from pathlib import Path
from datetime import datetime
import json
import re
import subprocess

blog = json.load(open('blog.json'))
posts = {}
for post in blog:
    name = post['name']
    if name in posts:
        if posts[name]['version'] >= post['version']:
            continue
    posts[name] = post

for post in posts.values():
    print(post['name'], post['version'])
    date = datetime.utcfromtimestamp(post['publish_time'])
    name = date.strftime(f'%Y%M%d-{post["name"]}')

    body = post['body'].replace('\\r', '').replace('\\n', '\n')
    body = re.sub(r'\[\[(https?://[^ |\]]+)\|([^\]]+)\]\]', r'[\2](\1)', body)
    body = re.sub(r'\[\[([^ |\]]+)\|([^\]]+)\]\]', r'[\2](https://gitlab.haskell.org/ghc/ghc/wikis/\1)', body)
    body = re.sub(r'\[(https?://[^ \]]+) ([^\]]+)\]', r'[\2](\1)', body)
    body = re.sub(r'\[(https?://[^ \]]+)\]', r'<\1>', body)
    body = re.sub(r'\[(wiki:[^ \]]+) ([^\]]+)\]', r'[\2](https://gitlab.haskell.org/ghc/ghc/wikis/\1)', body)
    body = re.sub(r"'''", '**', body)
    body = re.sub(r"''", '*', body)
    body = re.sub(r'{{{', '```haskell', body)
    body = re.sub(r'}}}', '```', body)
    for i in range(6, 1, -1):
        body = re.sub(r'{h} ([^=]*) {h}'.format(h='='*i), '#'*i + r' \1', body)
    #body = subprocess.check_output(['pandoc', '--from', 'creole', '--to', 'markdown'], input=body.encode('UTF-8')).decode('UTF-8')

    out_path = Path(name + ".md")
    out_path.parent.mkdir(exist_ok=True)
    with out_path.open('w') as out:
        out.write('---\n')
        out.write('author: ' + post['author'] + '\n')
        out.write('title: "' + post['title'] + '"\n')
        out.write('date: ' + date.isoformat() + '\n')
        out.write('tags: ' + post['categories'] + '\n')
        out.write('---\n\n')
        out.write(body)

    subprocess.check_call(['pandoc', '--to', 'html', '--standalone', '-o', out_path.with_suffix('.html'), out_path])

    Path(name + ".trac").write_text(post['body'])

