with tab1 as (
	select 
		col,
		other_col
	from table
),

tab2 as (
	select 
		c.name,
		c.mean
	from table_second as c
)

select col
from tab1 as c
