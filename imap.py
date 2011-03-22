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
    ('list-folders', dict(default=False, action='store_const', const=True)),
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
    parser.add_argument(
        '--%s' % name,
        dest=name.replace('-', '_'),
        **args)


def fetch_mail(M, msg_id, msg_part):
    ''' Fetch mail content '''
    typ, data = M.fetch(str(int(msg_id)), '(BODY.PEEK[' + msg_part + '])')
    data2 = data[0][1].decode()
    return {"mail-id": str(int(msg_id)), "content": data2}


def fetch_result(M, args):
    ''' Fetch result as specified in args '''
    M.select(args.folder, "true")
    typ, data = M.search(None, 'ALL')

    if args.list_folders:
        folders = M.list()
        if folders[0] == 'OK':
            return [folder.decode() for folder in folders[1]]

    if args.message_id is not None:
        return fetch_mail(M, args.message_id, args.message_part)
    else:
        mail_list = []
        for num in data[0].split():
            mail_list.append(fetch_mail(M, num, args.message_part))
        return mail_list


def to_json(result, args):
    ''' Convert result to JSON '''
    if args.humanize:
        json_args = dict(indent=True)
    else:
        json_args = dict(separators=(',', ':'))
    return json.dumps(result, **json_args)


def main():
    args = parser.parse_args()
    # Set message part
    message_part = 'TEXT'
    if args.headers_only:
        message_part = 'HEADER'
    setattr(args, 'message_part', message_part)

    # Init mailbox
    M = imaplib.IMAP4_SSL(args.host)
    # Get username and password
    usernm = args.username
    usernm = (usernm == '-' and getpass.getpass('Username: ')) or usernm
    passwd = args.password
    passwd = (passwd == '-' and getpass.getpass()) or passwd
    M.login(usernm, passwd)

    print(to_json(fetch_result(M, args), args))

    M.close()
    M.logout()


if __name__ == '__main__':
    main()
