<!-- 
	This is the Orders List page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<!-- Orders List header -->
		<a-row type="flex" :gutter="24">
			<a-col :span="12" class="mb-24">
				<a-button type="primary">NEW ORDER</a-button>
			</a-col>
			<a-col :span="12" class="mb-24 text-right">
				<a-dropdown :trigger="['click']">
					<a-button class="ant-dropdown-link" @click="e => e.preventDefault()">
						FILTERS <a-icon class="ml-5" type="down" />
					</a-button>
					<a-menu slot="overlay">
						<a-menu-item key="0">
							<a href="http://www.alipay.com/">status: Paid</a>
						</a-menu-item>
						<a-menu-item key="1">
							<a href="http://www.taobao.com/">status: Refunded</a>
						</a-menu-item>
						<a-menu-item key="2">
							<a href="http://www.taobao.com/">status: Canceled</a>
						</a-menu-item>
						<a-menu-divider />
						<a-menu-item key="3">
							<a href="http://www.taobao.com/" class="text-danger">Remove filters</a>
						</a-menu-item>
					</a-menu>
				</a-dropdown>
				<a-button @click="csvExport(csvData)" class="ml-15">
					<i class="ni ni-archive-2 mr-5"></i> EXPORT CSV
				</a-button>

			</a-col>
		</a-row>
		<!-- / Orders List header -->

		<!-- Orders List card -->
		<a-card :bordered="false" class="header-solid mb-24" :bodyStyle="{padding: 0, paddingTop: '16px'}">
			
			<!-- Table search -->
			<div class="mx-25">
				<a-row type="flex" :gutter="24">
					<a-col :span="24" class="text-right">
    					<a-input-search placeholder="input search text" style="max-width: 200px;" v-model="query" @change="onSearchChange" />
					</a-col>
				</a-row>
			</div>
			<!-- / Table search -->
			
			<!-- Orders table -->
			<a-table class="mt-20"
				:row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange }"
				:columns="columns"
				:data-source="data"
				:pagination="{pageSize: pageSize,}"
			>

				<template slot="id" slot-scope="id">#{{ id }}</template>

				<template slot="status" slot-scope="status">
    				<a-button v-if="status == 'Paid'" shape="circle" size="small" class="btn-status border-success mr-5">
						<a-icon class="m-0 text-success" type="check" style="font-size: 10px;" />
					</a-button>
    				<a-button v-else-if="status == 'Refunded'" shape="circle" size="small" class="btn-status border-muted mr-5">
						<a-icon class="m-0 text-muted" type="undo" style="font-size: 10px;" />
					</a-button>
    				<a-button v-else-if="status == 'Canceled'" shape="circle" size="small" class="btn-status border-danger mr-5">
						<a-icon class="m-0 text-danger" type="close" style="font-size: 10px;" />
					</a-button>
					<span style="vertical-align: middle;">{{ status }}</span>
				</template>

				<template slot="customer" slot-scope="customer">
					<div class="table-avatar-info">
						<a-avatar v-if="customer.avatar" shape="circle" :size="24" :src="customer.avatar" />
						<a-avatar v-else shape="circle" :size="24">{{ customer.name.slice(0, 1) }}</a-avatar>
						<div class="avatar-info">
							<p class="mb-0 text-dark">{{ customer.name }}</p>
						</div>
					</div>
				</template>

				<template slot="revenue" slot-scope="revenue">${{ revenue }}</template>

				<template slot="editBtn" slot-scope="row">
					<a-button type="link" :data-id="row.key" class="btn-edit">
						Edit
					</a-button>
				</template>

			</a-table>
			<!-- / Orders table -->

		</a-card>
		<!-- / Orders List card -->
		
	</div>

</template>

<script>

	// Sorting function for string attibutes.
	let stringSorter = function(a, b, attr) {
		if (a[attr] < b[attr])
			return -1;
		if ( a[attr] > b[attr])
			return 1;
		return 0;
	}

	// Table columns
	const columns = [
		{
			title: 'ID',
			dataIndex: 'key',
			sorter: (a, b) => a.key - b.key,
			sortDirections: ['descend', 'ascend'],
			scopedSlots: { customRender: 'id' },
		},
		{
			title: 'DATE',
			dataIndex: 'date',
			sorter: (a, b) => a.date.length - b.date.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'STATUS',
			dataIndex: 'status',
			sorter: (a, b) => stringSorter(a, b, 'status'),
			sortDirections: ['descend', 'ascend'],
			scopedSlots: { customRender: 'status' },
		},
		{
			title: 'CUSTOMER',
			dataIndex: 'customer',
			sorter(a, b, attr) {
				if (a.customer.name < b.customer.name)
					return -1;
				if ( a.customer.name > b.customer.name)
					return 1;
				return 0;
			},
			sortDirections: ['descend', 'ascend'],
			scopedSlots: { customRender: 'customer' },
		},
		{
			title: 'PRODUCT',
			dataIndex: 'product',
			sorter: (a, b) => stringSorter(a, b, 'product'),
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'REVENUE',
			dataIndex: 'revenue',
			sorter: (a, b) => parseFloat(a.revenue) - parseFloat(b.revenue),
			sortDirections: ['descend', 'ascend'],
			scopedSlots: { customRender: 'revenue' },
		},
	];

	// Table rows
	const data = [
		{
			"key": 10421,
			"date": "1 Nov, 10:20 AM",
			"status": "Paid",
			"customer": {
				"name": "Orlando Imieto",
				"avatar": "images/team-2.jpg",
			},
			"product": "Nike Sport V2",
			"revenue": "140.20",
		},
		{
			"key": 10422,
			"date": "1 Nov, 10:53 AM",
			"status": "Paid",
			"customer": {
				"name": "Alice Murinho",
				"avatar": "images/team-1.jpg",
			},
			"product": "Valvet T-shirt",
			"revenue": "42.00",
		},
		{
			"key": 10423,
			"date": "1 Nov, 11:13 AM",
			"status": "Refunded",
			"customer": {
				"name": "Michael Mirra",
			},
			"product": "Leather Wallet",
			"extra": "+1 more",
			"revenue": "25.50",
		},
		{
			"key": 10424,
			"date": "1 Nov, 12:20 PM",
			"status": "Paid",
			"customer": {
				"name": "Andrew Nichel",
				"avatar": "images/team-3.jpg",
			},
			"product": "Bracelet Onu-Lino",
			"revenue": "19.40",
		},
		{
			"key": 10425,
			"date": "1 Nov, 1:40 PM",
			"status": "Canceled",
			"customer": {
				"name": "Sebastian Koga",
				"avatar": "images/team-4.jpg",
			},
			"product": "Phone Case Pink",
			"extra": "x 2",
			"revenue": "44.90",
		},
		{
			"key": 10426,
			"date": "1 Nov, 2:19 AM",
			"status": "Paid",
			"customer": {
				"name": "Laur Gilbert",
			},
			"product": "Backpack Niver",
			"revenue": "112.50",
		},
		{
			"key": 10427,
			"date": "1 Nov, 3:42 AM",
			"status": "Paid",
			"customer": {
				"name": "Iryna Innda",
			},
			"product": "Adidas Vio",
			"revenue": "200.00",
		},
		{
			"key": 10428,
			"date": "2 Nov, 9:32 AM",
			"status": "Paid",
			"customer": {
				"name": "Arrias Liunda",
			},
			"product": "Airpods 2 Gen",
			"revenue": "350.00",
		},
		{
			"key": 10429,
			"date": "2 Nov, 10:14 AM",
			"status": "Paid",
			"customer": {
				"name": "Rugna Ilpio",
				"avatar": "images/team-5.jpg",
			},
			"product": "Bracelet Warret",
			"revenue": "15.00",
		},
		{
			"key": 10430,
			"date": "2 Nov, 12:56 PM",
			"status": "Refunded",
			"customer": {
				"name": "Anna Landa",
				"avatar": "images/ivana-squares.jpg",
			},
			"product": "Watter Bottle India",
			"extra": "x 3",
			"revenue": "25.00",
		},
		{
			"key": 10431,
			"date": "2 Nov, 3:12 PM",
			"status": "Paid",
			"customer": {
				"name": "Karl Innas",
			},
			"product": "Kitchen Gadgets",
			"revenue": "164.90",
		},
		{
			"key": 10432,
			"date": "2 Nov, 5:12 PM",
			"status": "Paid",
			"customer": {
				"name": "Oana Kilas",
			},
			"product": "Office Papers",
			"revenue": "23.90",
		},
	];

	export default {
		data() {
			return {
				
				// Table columns
				columns,
				
				// Table rows
				data,
				
				// Table page size
				pageSize: 10,
				
				// Table search query
				query: '',
				
				// Table's selected rows
      			selectedRowKeys: [],

			}
		},
		computed: {
			
			// CSV data array
			csvData() {
				return this.data.map(item => ({
					"Id": "%23" + item.key,
					"Date": item.date,
					"Status": item.status,
					"Customer": item.customer.name,
					"Product": item.product,
					"Revenue": "$" + item.revenue,
				}));
			}

		},
		methods: {

			// Event listener for input change on table search field.
			onSearchChange() {
				if( this.query.length > 0 ) {
					this.data = data.filter( (row) => {
						for( const key in row ) {
							if( row[ key ]
								.toString()
								.toLowerCase()
								.includes( this.query.trim().toLowerCase() ) )
								return true;
						}
						return false;
					}) ;
				}
				else {
					this.data = data ;
				}
			},

			// Event listener for table row selection change.
			onSelectChange(selectedRowKeys) {
				this.selectedRowKeys = selectedRowKeys;
			},

			// Export table in CSV format.
			csvExport(arrData) {
				let csvContent = "data:text/csv;charset=utf-8,";
				csvContent += [
					Object.keys(arrData[0]).join("|"),
					...arrData.map(item => Object.values(item).join("|"))
				]
					.join("\n")
					.replace(/(^\[)|(\]$)/gm, "");

				const data = encodeURI(csvContent);
				const link = document.createElement("a");
				link.setAttribute("href", data);
				link.setAttribute("download", "muse-dashboard-csv.csv");
				link.click();
			}

		},
	}

</script>

<style lang="scss" scoped>
	.table-avatar-info {
		display: flex;
		align-items: center;
	}
	.table-avatar-info .ant-avatar {
		margin-right: 8px;
	}

	// Using vuejs "Deep Selectors"
	.table-avatar-info::v-deep .ant-avatar-string {
		font-size: 12px;
	}
	.btn-status::v-deep .anticon {
		line-height: 0;
	}
</style>