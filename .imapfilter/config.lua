function main()
	local account = IMAP {
		server = 'foo.antani.work',
		username = 'wintermute',
		password = get_imap_password(".offlineimap_pass"),
		ssl = 'tls1',
	}

	-- Make sure the account is configured properly
	account.INBOX:check_status()
	account['INBOX.misc_openbsd']:check_status()
	account['INBOX.tech_openbsd']:check_status()


	-- Get all mail from INBOX
	mails = account.INBOX:select_all()

	-- Move mailing lists from INBOX to correct folders
	move_mailing_lists(account, mails)

	-- Get all mail from trash
	--mails = account['[Gmail]/Trash']:select_all()
end

function move_mailing_lists(account, mails)
	move_if_to_contains(account, mails, "misc@openbsd.org", "INBOX.misc_openbsd")
end

function move_if_subject_contains(account, mails, subject, mailbox)
	filtered = mails:contain_subject(subject)
	filtered:move_messages(account[mailbox]);
end

function move_if_to_contains(account, mails, to, mailbox)
	filtered = mails:contain_to(to)
	filtered:move_messages(account[mailbox]);
end

function move_if_from_contains(account, mails, from, mailbox)
	filtered = mails:contain_from(from)
	filtered:move_messages(account[mailbox]);
end

function delete_mail_from(account, mails, from)
	filtered = mails:contain_from(from)
	filtered:delete_messages()
end

function delete_mail_if_subject_contains(account, mails, subject)
	filtered = mails:contain_subject(subject)
	filtered:delete_messages()
end

-- Utility function to get IMAP password from file
function get_imap_password(file)
	local home = os.getenv("HOME")
	local file = home .. "/" .. file
	local str = io.open(file):read()
	return str;
end

main() -- Call the main function
