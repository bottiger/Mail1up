#!/usr/bin/python3.2

import imaplib
import getopt
import sys
import json

username = "jmailbackup44@gmail.com"
password = "qwerty60"
server = "imap.gmail.com"
folder = "INBOX"
headers_only = False
msg_id = 0

opts, remainder = getopt.getopt(sys.argv[1:], 'u:p:s:f:a:', ['username=', 
                                                         'password=',
                                                         'server=',
							 'folder=',
							 'after=',
							 'headers-only',
							 'msg-id='
                                                         ])

for opt, arg in opts:                
        if opt in ("-u", "--username"):      
            username = arg                
        elif opt in ("-p", "--password"):
            password = arg
        elif opt in ("-s", "--server"): 
            server = arg
        elif opt in ("-f", "--folder"):
            folder = arg
        elif opt in ("a", "--after"):
            start_date = arg
        elif opt in ("--headers-only"):
            headers_only = True
        elif opt in ("--msg-id"):
            msg_id = str(int(arg))

if headers_only:
    message_parts = "HEADER"
else:
    message_parts = "TEXT"

def fetch_mail(M, msg_id, msg_parts, mail_list):
    typ, data = M.fetch(msg_id, '(BODY.PEEK[' + msg_parts + '])')
    data2 = data[0][1].decode()
    mail_list.append({"mail-id": str(int(msg_id)), "content": data2})
    return mail_list


M = imaplib.IMAP4_SSL(server)
M.login(username, password)
M.list
M.select(folder, "true")
typ, data = M.search(None, 'ALL')

mail_list = []

if msg_id:
    mail_list = fetch_mail(M, msg_id, message_parts, mail_list)
else:
    for num in data[0].split():
        mail_list = fetch_mail(M, num, message_parts, mail_list)
        ##typ, data = M.fetch(num, '(BODY.PEEK[' + message_parts + '])')
        ##data2 = data[0][1].decode()
        ##mail_list.append({"mail-id": str(int(num)), "headers": data2})
        #mail_list.append({int(num): data2})
        #print('%s\n' % data[0][1].decode())

print(json.dumps(mail_list, separators=(',',':') ))

M.close()
M.logout()
