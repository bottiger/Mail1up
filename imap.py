#!/usr/bin/env python3

import imaplib
import json
import getpass
from argparse import ArgumentParser

# List of arguments for ArgumentParser
arguments = [
    ('username', dict(default='jmailbackup44@gmail.com')),
    ('password', dict(default='qwerty60')),
    ('host', dict(default='imap.gmail.com')),
    ('folder', dict(default='INBOX')),
    ('after', dict(type=str)),
    ('headers-only', dict(default=False, action='store_const', const=True)),
    ('humanize', dict(default=False, action='store_const', const=True)),
    ('message-id', dict(type=int)),
    ]


def unpack_argument(args):
    ''' Convenient argument wrapper '''
    if not 'const' in args:
        if 'default' in args and not 'type' in args:
            # find type from default value
            args['type'] = type(args['default'])
    return args

# Build argument parser
parser = ArgumentParser(description='IMAP wrapper.')
for name, args in arguments:
    args = unpack_argument(args)
    print(args)
    parser.add_argument('--%s' % name,
                        dest=name.replace('-', '_'),
                        **args)
args = parser.parse_args()

# Set message part
message_part = 'TEXT'
if args.headers_only:
    message_part = "HEADER"


def fetch_mail(M, msg_id, msg_part):
    ''' Fetch mail content '''
    typ, data = M.fetch(str(int(msg_id)), '(BODY.PEEK[' + msg_part + '])')
    data2 = data[0][1].decode()
    return {"mail-id": str(int(msg_id)), "content": data2}


# Init mailbox
M = imaplib.IMAP4_SSL(args.host)
# Get username and password
usernm = args.username
usernm = (usernm == '-' and getpass.getpass('Username: ')) or usernm
passwd = args.password
passwd = (passwd == '-' and getpass.getpass()) or passwd
M.login(usernm, passwd)
M.select(args.folder, "true")
typ, data = M.search(None, 'ALL')

mail_list = []

if args.message_id is not None:
    mail_list.append(fetch_mail(M, args.message_id, message_part))
else:
    for num in data[0].split():
        mail_list.append(fetch_mail(M, num, message_part))
        ##typ, data = M.fetch(num, '(BODY.PEEK[' + message_parts + '])')
        ##data2 = data[0][1].decode()
        ##mail_list.append({"mail-id": str(int(num)), "headers": data2})
        #mail_list.append({int(num): data2})
        #print('%s\n' % data[0][1].decode())

if args.humanize:
    print(json.dumps(mail_list, indent=True))
else:
    print(json.dumps(mail_list, separators=(',', ':')))

M.close()
M.logout()
