insert into {parametrized_table}

select 
    f.id,
    f.level,
	count(*) as n
from {i_dont_know} as f
group by 1, 2
order by 1, 2
