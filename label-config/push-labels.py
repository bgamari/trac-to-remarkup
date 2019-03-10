#!/usr/bin/env python3

import sys
import gitlab
import yaml

gl = gitlab.Gitlab.from_config('haskell')
proj = gl.projects.get(1)
existing = proj.labels.list(all=True)
existing = { label.name: label for label in existing }

for label in yaml.load(open('labels.yaml')):
    print(label)
    if label['name'] in existing:
        l =  existing[label['name']]
        l.color = label['color']
        l.description = label['description']
        l.save()
    else:
        #proj.labels.create(label)
        print("doesn't exist")

