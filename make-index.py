#!/usr/bin/env python3

from pathlib import Path
from typing import List, Dict

root = Path.cwd()

class WikiName(object):
    def __init__(self, name: str) -> None:
        self.name = name
        self.children = {}  # type: Dict[str, WikiName]
        self.is_page = False

    def child(self, name: str) -> "WikiName":
        self.children.setdefault(name, WikiName(name))
        return self.children[name]

files = WikiName('GHC Wiki') # type: WikiName
for f in root.rglob('*.md'):
    f = f.relative_to(root)

    x = files # type: WikiName
    for p in reversed(list(f.parents)[:-1]):
        x = x.child(p.name)

    x = x.child(f.stem)
    x.is_page = True
    
def make_index(path: List[str], x: WikiName, depth: int) -> List[str]:
    indent = '  ' * depth
    name = x.name
    if x.is_page:
        target = '/'.join(path)
        name = f'[{name}](./{target})'

    entries = [f'{indent}* {name}']
    for _,y in sorted(x.children.items()):
        entries += make_index(path + [y.name], y, depth+1)
    return entries

index = make_index([], files, 0)
index = '\n'.join(index)
open(root / 'index.md', 'w').write(index)
print('index written.')
