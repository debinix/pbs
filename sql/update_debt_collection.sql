update tutorials.pbs.invoice
	set invstate = 4 
	where inv_id in
		select inv_id
			from invoice
			where invstate = 2
			and TODAY - invdate > 15
