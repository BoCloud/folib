<template>
				
	<!-- Sales By Country Table Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{padding: 0,}">
		<template #title>
			<a-row type="flex" align="middle">
				<a-col :span="24" :md="12">
					<h6>Sales by Country</h6>			
				</a-col>
				<a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
				</a-col>
			</a-row>
		</template>
		<a-table :columns="columns" :showHeader="false" :data-source="data" :pagination="false">

			<a-space slot="members" slot-scope="members" :size="-12" class="avatar-chips">
				<template v-for="member in members">
					<a-avatar :key="member" size="small" :src="member" />
				</template>
			</a-space>

			<template slot="flag" slot-scope="flag">
				<img :src="flag" width="25">
			</template>

			<template slot="country" slot-scope="country">
				<p class="m-0 text-sm font-semibold text-muted">Country:</p>
				<h6 class="m-0">{{ country }}</h6>
			</template>

			<template slot="sales" slot-scope="sales">
				<p class="m-0 text-sm font-semibold text-muted">Sales:</p>
				<h6 class="m-0">{{ sales }}</h6>
			</template>

			<template slot="value" slot-scope="value">
				<p class="m-0 text-sm font-semibold text-muted">Value:</p>
				<h6 class="m-0">{{ value }}</h6>
			</template>

			<template slot="bounce" slot-scope="bounce">
				<p class="m-0 text-sm font-semibold text-muted">Bounce:</p>
				<h6 class="m-0">{{ bounce }}</h6>
			</template>

		</a-table>
	</a-card>
	<!-- / Sales By Country Table Card -->

</template>

<script>

	const tableColumns = [
		{
			title: '',
			dataIndex: 'flag',
			scopedSlots: { customRender: 'flag' },
			width: 50,
			class: "p-15"
		},
		{
			title: '',
			dataIndex: 'country',
			scopedSlots: { customRender: 'country' },
			class: "pl-10"
		},
		{
			title: '',
			dataIndex: 'sales',
			scopedSlots: { customRender: 'sales' },
		},
		{
			title: '',
			dataIndex: 'value',
			scopedSlots: { customRender: 'value' },
		},
		{
			title: '',
			dataIndex: 'bounce',
			scopedSlots: { customRender: 'bounce' },
		},
	];

	const tableData = [
		{
			key: '1',
			flag: 'images/icons/flags/US.png',
			country: 'United States',
			sales: '2,500',
			value: '$230,900',
			bounce: '29.9%',
		},
		{
			key: '2',
			flag: 'images/icons/flags/DE.png',
			country: 'Germany',
			sales: '3,900',
			value: '$440,000',
			bounce: '40.22%',
		},
		{
			key: '3',
			flag: 'images/icons/flags/GB.png',
			country: 'Great Britain',
			sales: '1,400',
			value: '$190,700',
			bounce: '23.44%',
		},
		{
			key: '4',
			flag: 'images/icons/flags/BR.png',
			country: 'Brazil',
			sales: '562',
			value: '$143,960',
			bounce: '32.14%',
		},
	];

	export default ({
		props: {
		},
		data() {
			return {

				columns: tableColumns,
				data: tableData,
			}
		},
	})

</script>