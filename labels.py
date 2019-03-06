#!/usr/bin/env python3

import sys
import gitlab
import yaml

gl = gitlab.Gitlab.from_config('haskell-staging')
proj = gl.projects.get(1)
labels = proj.labels.list(all=True)
labels = \
    [ { 'name': label.name, 'color': label.color, 'description': label.description }
      for label in labels ]
    
yaml.dump(labels, sys.stdout, default_flow_style=False)

