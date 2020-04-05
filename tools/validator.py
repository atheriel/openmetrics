#!/usr/bin/python

import sys
from prometheus_client.openmetrics import parser

text = sys.stdin.read().decode("utf-8")
for families in parser.text_string_to_metric_families(text):
    print families
