update tutorials.pbs.invoice
	set invstate = 2 
	where inv_id in
		select inv_id
			from invoice
			where invstate = 1
			and TODAY - invdate > 30

update tutorials.pbs.invoice
	set invdate = TODAY
	where invstate = 2
