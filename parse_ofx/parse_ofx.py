# -*- coding: utf-8 -*-
#!/usr/bin/python
# @author Arnaldo Vitaliano (vitaliano@gmail.com)
# This script parses ofx to csv using ofxparse library

import os, glob
import string
from ofxparse import OfxParser

def parse_ofx(f):

    output_filename = f.replace('.ofx', '.csv')

    print 'parsing file ' + f
    # Realiza o parse do arquivo ofx. Pega exceção de caractere inválido.
    try:
      ofx = OfxParser.parse(open(f, 'r'))
    except UnicodeDecodeError:
      dirty = open(f, 'r').read()
      temp = open(f + '.tmp', 'w')
      filtered_string = filter(lambda x: x in string.printable, dirty)
      temp.write(filtered_string)
      temp.flush()
      temp.close()
      ofx = OfxParser.parse(open(temp.name, 'r'), fail_fast=False)
      os.remove(temp.name)
      
    wf = open(output_filename, 'w')

    for ac in ofx.accounts:

      wf.write( '%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s\n' %(
          'inst.fid',
          'inst.organization',
          'ac.account_id',
          'ac.account_type',
          'ac.branch_id',
          'ac.number',
          'ac.type',
          'stat.balance',
          'stat.currency',
          'stat.start_date', 
          'stat.end_date', 
          't.checknum',
          't.id',
          't.date', 
          't.amount',  
          't.payee', 
          't.type',
          't.memo'))

      inst = ac.institution
      stat = ac.statement
      
      for t in stat.transactions:
         line =  '%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;' %(
          inst.fid,
          inst.organization,
          ac.account_id,
          ac.account_type,
          ac.branch_id,
          ac.number,
          ac.type,
          stat.balance,
          stat.currency.upper(),
          stat.start_date.strftime('%Y-%m-%d'), 
          stat.end_date.strftime('%Y-%m-%d'), 
          t.checknum,
          t.id,
          t.date.strftime('%Y-%m-%d'), 
          t.amount, 
          t.payee,
          t.type.upper())
         wf.write(line)
         wf.write(t.memo.encode('utf-8'))
         wf.write('\n')

    wf.close()

def list_ofx_files(dir):

  return_list = []
  for root, dirs, files in os.walk(dir):
    for file in files:
        if file.endswith(".ofx"):
             return_list.append(os.path.join(root, file))

  return return_list

for ofx in list_ofx_files('/Users/arnaldo/Documents/SpiderOak/Financeiro/ofx/SANTANDER'):
  parse_ofx(ofx)
