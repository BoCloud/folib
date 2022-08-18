<!-- 
	This is the DataTables page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<!-- Simple Datatable card -->
		<a-card :bordered="false" class="header-solid mb-24" :bodyStyle="{padding: 0, paddingTop: '16px'}">
			<template #title>
				<h5 class="font-semibold">Datatable Simple</h5>
			</template>

			<div class="mx-25">
				<a-row type="flex" :gutter="24">
					<a-col :span="24" md="12">
						<a-select v-model="pageSize" @change="onPageSizeChange" style="width: 70px">
							<a-select-option value="5">5</a-select-option>
							<a-select-option value="10">10</a-select-option>
							<a-select-option value="15">15</a-select-option>
							<a-select-option value="20">20</a-select-option>
							<a-select-option value="25">25</a-select-option>
						</a-select>
						<label for="" class="ml-10">entries per page</label>
					</a-col>
					<a-col :span="24" md="12">
						
					</a-col>
				</a-row>
			</div>
			
			<a-table class="mt-20" :columns="columns" :data-source="data" :pagination="{pageSize: pageSize,}" />
		</a-card>
		<!-- / Simple Datatable card -->

		<!-- Searchable Datatable card -->
		<a-card :bordered="false" class="header-solid mb-24" :bodyStyle="{padding: 0, paddingTop: '16px'}">
			<template #title>
				<h5 class="font-semibold">Datatable Search</h5>
			</template>

			<div class="mx-25">
				<a-row type="flex" :gutter="24">
					<a-col :span="24" :md="12">
						<a-select v-model="pageSize2" @change="onPageSize2Change" style="width: 70px">
							<a-select-option value="5">5</a-select-option>
							<a-select-option value="10">10</a-select-option>
							<a-select-option value="15">15</a-select-option>
							<a-select-option value="20">20</a-select-option>
							<a-select-option value="25">25</a-select-option>
						</a-select>
						<label for="" class="ml-10">entries per page</label>
					</a-col>
					<a-col :span="24" :md="12" class="text-right">
    					<a-input-search placeholder="input search text" style="max-width: 200px;" v-model="query" @change="onSearchChange" />
					</a-col>
				</a-row>
			</div>
			
			<a-table class="mt-20" :columns="columns2" :data-source="data2" :pagination="{pageSize: pageSize2,}" />
		</a-card>
		<!-- / Searchable Datatable card -->
		
	</div>

</template>

<script>

	// First table's list of columns.
	const columns = [
		{
			title: 'NAME',
			dataIndex: 'name',
			sorter: (a, b) => a.name.length - b.name.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'AGE',
			dataIndex: 'age',
			sortDirections: ['descend', 'ascend'],
			sorter: (a, b) => a.age - b.age,
		},
		{
			title: 'ADDRESS',
			dataIndex: 'address',
			sorter: (a, b) => a.address.length - b.address.length,
			sortDirections: ['descend', 'ascend'],
		},
	];

	// First table's list of rows.
	const data = [
		{
			key: '1',
			name: 'John Brown',
			age: 32,
			address: 'New York No. 1 Lake Park',
		},
		{
			key: '2',
			name: 'Jim Green',
			age: 42,
			address: 'London No. 1 Lake Park',
		},
		{
			key: '3',
			name: 'Joe Black',
			age: 32,
			address: 'Sidney No. 1 Lake Park',
		},
		{
			key: '4',
			name: 'Jim Red',
			age: 32,
			address: 'London No. 2 Lake Park',
		},
	];

	// Second table's list of columns.
	const columns2 = [
		{
			title: 'NAME',
			dataIndex: 'name',
			sorter: (a, b) => a.name.length - b.name.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'POSITION',
			dataIndex: 'position',
			sortDirections: ['descend', 'ascend'],
			sorter: (a, b) => a.position - b.position,
		},
		{
			title: 'OFFICE',
			dataIndex: 'office',
			sorter: (a, b) => a.office.length - b.office.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'AGE',
			dataIndex: 'age',
			sorter: (a, b) => a.age.length - b.age.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'START DATE',
			dataIndex: 'startDate',
			sorter: (a, b) => a.startDate.length - b.startDate.length,
			sortDirections: ['descend', 'ascend'],
		},
		{
			title: 'SALARY',
			dataIndex: 'salary',
			sorter: (a, b) => a.salary.length - b.salary.length,
			sortDirections: ['descend', 'ascend'],
		},
	];
	
	// Second table's list of rows.
	const data2 = [
		{
			key: 1,
			name: "Tiger Nixon",
			position: "System Architect",
			office: "Edinburgh",
			age: "21",
			startDate: "2011/04/25",
			salary: "$320,800"
		},
		{
			key: 2,
			name: "Garrett Winters",
			position: "Accountant",
			office: "Tokyo",
			age: "22",
			startDate: "2011/07/25",
			salary: "$170,750"
		},
		{
			key: 3,
			name: "Ashton Cox",
			position: "Junior Technical Author",
			office: "San Francisco",
			age: "62",
			startDate: "2009/01/12",
			salary: "$86,000"
		},
		{
			key: 4,
			name: "Cedric Kelly",
			position: "Senior Javascript Developer",
			office: "Edinburgh",
			age: "24",
			startDate: "2012/03/29",
			salary: "$433,060"
		},
		{
			key: 5,
			name: "Airi Satou",
			position: "Accountant",
			office: "Tokyo",
			age: "07",
			startDate: "2008/11/28",
			salary: "$162,700"
		},
		{
			key: 6,
			name: "Brielle Williamson",
			position: "Integration Specialist",
			office: "New York",
			age: "04",
			startDate: "2012/12/02",
			salary: "$372,000"
		},
		{
			key: 7,
			name: "Herrod Chandler",
			position: "Sales Assistant",
			office: "San Francisco",
			age: "08",
			startDate: "2012/08/06",
			salary: "$137,500"
		},
		{
			key: 8,
			name: "Rhona Davidson",
			position: "Integration Specialist",
			office: "Tokyo",
			age: "00",
			startDate: "2010/10/14",
			salary: "$327,900"
		},
		{
			key: 9,
			name: "Colleen Hurst",
			position: "Javascript Developer",
			office: "San Francisco",
			age: "60",
			startDate: "2009/09/15",
			salary: "$205,500"
		},
		{
			key: 10,
			name: "Sonya Frost",
			position: "Software Engineer",
			office: "Edinburgh",
			age: "67",
			startDate: "2008/12/13",
			salary: "$103,600"
		},
		{
			key: 11,
			name: "Jena Gaines",
			position: "Office Manager",
			office: "London",
			age: "14",
			startDate: "2008/12/19",
			salary: "$90,560"
		},
		{
			key: 12,
			name: "Quinn Flynn",
			position: "Support Lead",
			office: "Edinburgh",
			age: "97",
			startDate: "2013/03/03",
			salary: "$342,000"
		},
		{
			key: 13,
			name: "Charde Marshall",
			position: "Regional Director",
			office: "San Francisco",
			age: "41",
			startDate: "2008/10/16",
			salary: "$470,600"
		},
		{
			key: 14,
			name: "Haley Kennedy",
			position: "Senior Marketing Designer",
			office: "London",
			age: "97",
			startDate: "2012/12/18",
			salary: "$313,500"
		},
		{
			key: 15,
			name: "Tatyana Fitzpatrick",
			position: "Regional Director",
			office: "London",
			age: "65",
			startDate: "2010/03/17",
			salary: "$385,750"
		},
		{
			key: 16,
			name: "Michael Silva",
			position: "Marketing Designer",
			office: "London",
			age: "81",
			startDate: "2012/11/27",
			salary: "$198,500"
		},
		{
			key: 17,
			name: "Paul Byrd",
			position: "Chief Financial Officer (CFO)",
			office: "New York",
			age: "59",
			startDate: "2010/06/09",
			salary: "$725,000"
		},
		{
			key: 18,
			name: "Gloria Little",
			position: "Systems Administrator",
			office: "New York",
			age: "21",
			startDate: "2009/04/10",
			salary: "$237,500"
		},
		{
			key: 19,
			name: "Bradley Greer",
			position: "Software Engineer",
			office: "London",
			age: "58",
			startDate: "2012/10/13",
			salary: "$132,000"
		},
		{
			key: 20,
			name: "Dai Rios",
			position: "Personnel Lead",
			office: "Edinburgh",
			age: "90",
			startDate: "2012/09/26",
			salary: "$217,500"
		},
		{
			key: 21,
			name: "Jenette Caldwell",
			position: "Development Lead",
			office: "New York",
			age: "37",
			startDate: "2011/09/03",
			salary: "$345,000"
		},
		{
			key: 22,
			name: "Yuri Berry",
			position: "Chief Marketing Officer (CMO)",
			office: "New York",
			age: "54",
			startDate: "2009/06/25",
			salary: "$675,000"
		},
		{
			key: 23,
			name: "Caesar Vance",
			position: "Pre-Sales Support",
			office: "New York",
			age: "30",
			startDate: "2011/12/12",
			salary: "$106,450"
		},
		{
			key: 24,
			name: "Doris Wilder",
			position: "Sales Assistant",
			office: "Sydney",
			age: "23",
			startDate: "2010/09/20",
			salary: "$85,600"
		},
		{
			key: 25,
			name: "Angelica Ramos",
			position: "Chief Executive Officer (CEO)",
			office: "London",
			age: "97",
			startDate: "2009/10/09",
			salary: "$1,200,000"
		},
		{
			key: 26,
			name: "Gavin Joyce",
			position: "Developer",
			office: "Edinburgh",
			age: "22",
			startDate: "2010/12/22",
			salary: "$92,575"
		},
		{
			key: 27,
			name: "Jennifer Chang",
			position: "Regional Director",
			office: "Singapore",
			age: "39",
			startDate: "2010/11/14",
			salary: "$357,650"
		},
		{
			key: 28,
			name: "Brenden Wagner",
			position: "Software Engineer",
			office: "San Francisco",
			age: "14",
			startDate: "2011/06/07",
			salary: "$206,850"
		},
		{
			key: 29,
			name: "Fiona Green",
			position: "Chief Operating Officer (COO)",
			office: "San Francisco",
			age: "47",
			startDate: "2010/03/11",
			salary: "$850,000"
		},
		{
			key: 30,
			name: "Shou Itou",
			position: "Regional Marketing",
			office: "Tokyo",
			age: "99",
			startDate: "2011/08/14",
			salary: "$163,000"
		},
		{
			key: 31,
			name: "Michelle House",
			position: "Integration Specialist",
			office: "Sydney",
			age: "69",
			startDate: "2011/06/02",
			salary: "$95,400"
		},
		{
			key: 32,
			name: "Suki Burks",
			position: "Developer",
			office: "London",
			age: "32",
			startDate: "2009/10/22",
			salary: "$114,500"
		},
		{
			key: 33,
			name: "Prescott Bartlett",
			position: "Technical Author",
			office: "London",
			age: "06",
			startDate: "2011/05/07",
			salary: "$145,000"
		},
		{
			key: 34,
			name: "Gavin Cortez",
			position: "Team Leader",
			office: "San Francisco",
			age: "60",
			startDate: "2008/10/26",
			salary: "$235,500"
		},
		{
			key: 35,
			name: "Martena Mccray",
			position: "Post-Sales support",
			office: "Edinburgh",
			age: "40",
			startDate: "2011/03/09",
			salary: "$324,050"
		},
		{
			key: 36,
			name: "Unity Butler",
			position: "Marketing Designer",
			office: "San Francisco",
			age: "84",
			startDate: "2009/12/09",
			salary: "$85,675"
		},
		{
			key: 37,
			name: "Howard Hatfield",
			position: "Office Manager",
			office: "San Francisco",
			age: "31",
			startDate: "2008/12/16",
			salary: "$164,500"
		},
		{
			key: 38,
			name: "Hope Fuentes",
			position: "Secretary",
			office: "San Francisco",
			age: "18",
			startDate: "2010/02/12",
			salary: "$109,850"
		},
		{
			key: 39,
			name: "Vivian Harrell",
			position: "Financial Controller",
			office: "San Francisco",
			age: "22",
			startDate: "2009/02/14",
			salary: "$452,500"
		},
		{
			key: 40,
			name: "Timothy Mooney",
			position: "Office Manager",
			office: "London",
			age: "80",
			startDate: "2008/12/11",
			salary: "$136,200"
		},
		{
			key: 41,
			name: "Jackson Bradshaw",
			position: "Director",
			office: "New York",
			age: "42",
			startDate: "2008/09/26",
			salary: "$645,750"
		},
		{
			key: 42,
			name: "Olivia Liang",
			position: "Support Engineer",
			office: "Singapore",
			age: "20",
			startDate: "2011/02/03",
			salary: "$234,500"
		},
		{
			key: 43,
			name: "Bruno Nash",
			position: "Software Engineer",
			office: "London",
			age: "22",
			startDate: "2011/05/03",
			salary: "$163,500"
		},
		{
			key: 44,
			name: "Sakura Yamamoto",
			position: "Support Engineer",
			office: "Tokyo",
			age: "83",
			startDate: "2009/08/19",
			salary: "$139,575"
		},
		{
			key: 45,
			name: "Thor Walton",
			position: "Developer",
			office: "New York",
			age: "27",
			startDate: "2013/08/11",
			salary: "$98,540"
		},
		{
			key: 46,
			name: "Finn Camacho",
			position: "Support Engineer",
			office: "San Francisco",
			age: "27",
			startDate: "2009/07/07",
			salary: "$87,500"
		},
		{
			key: 47,
			name: "Serge Baldwin",
			position: "Data Coordinator",
			office: "Singapore",
			age: "52",
			startDate: "2012/04/09",
			salary: "$138,575"
		},
		{
			key: 48,
			name: "Zenaida Frank",
			position: "Software Engineer",
			office: "New York",
			age: "39",
			startDate: "2010/01/04",
			salary: "$125,250"
		},
		{
			key: 49,
			name: "Zorita Serrano",
			position: "Software Engineer",
			office: "San Francisco",
			age: "89",
			startDate: "2012/06/01",
			salary: "$115,000"
		},
		{
			key: 50,
			name: "Jennifer Acosta",
			position: "Junior Javascript Developer",
			office: "Edinburgh",
			age: "31",
			startDate: "2013/02/01",
			salary: "$75,650"
		},
		{
			key: 51,
			name: "Cara Stevens",
			position: "Sales Assistant",
			office: "New York",
			age: "90",
			startDate: "2011/12/06",
			salary: "$145,600"
		},
		{
			key: 52,
			name: "Hermione Butler",
			position: "Regional Director",
			office: "London",
			age: "16",
			startDate: "2011/03/21",
			salary: "$356,250"
		},
		{
			key: 53,
			name: "Lael Greer",
			position: "Systems Administrator",
			office: "London",
			age: "33",
			startDate: "2009/02/27",
			salary: "$103,500"
		},
		{
			key: 54,
			name: "Jonas Alexander",
			position: "Developer",
			office: "San Francisco",
			age: "96",
			startDate: "2010/07/14",
			salary: "$86,500"
		},
		{
			key: 55,
			name: "Shad Decker",
			position: "Regional Director",
			office: "Edinburgh",
			age: "73",
			startDate: "2008/11/13",
			salary: "$183,000"
		},
		{
			key: 56,
			name: "Michael Bruce",
			position: "Javascript Developer",
			office: "Singapore",
			age: "84",
			startDate: "2011/06/27",
			salary: "$183,000"
		},
		{
			key: 57,
			name: "Donna Snider",
			position: "Customer Support",
			office: "New York",
			age: "26",
			startDate: "2011/01/25",
			salary: "$112,000"
		}
	] ;

	export default {
		components: {
		},
		data() {
			return {

				// First table's list of columns.
				columns,

				// First table's list of rows.
				data,

				// Second table's list of columns.
				columns2,
	
				// Second table's list of rows.
				data2,

				// First table's number of rows per page.
				pageSize: 10,

				// Second table's number of rows per page.
				pageSize2: 10,
				
				// Second table's search query.
				query: '',

			}
		},
		methods: {

			// Event handler for first table's size change.
			onPageSizeChange() {
				this.pageSize = parseInt( this.pageSize ) ;
			},

			// Event handler for second table's size change.
			onPageSize2Change() {
				this.pageSize2 = parseInt( this.pageSize2 ) ;
			},

			// Event handler for second table's search.
			onSearchChange() {
				if( this.query.length > 0 ) {
					this.data2 = data2.filter( (row) => {
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
					this.data2 = data2 ;
				}
			},
			
		},
	}

</script>

<style lang="scss">
</style>