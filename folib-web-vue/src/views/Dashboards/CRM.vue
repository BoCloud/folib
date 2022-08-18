<!-- 
	This is the Dashboards CRM page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>
		
		<!-- Charts -->
		<a-row :gutter="24" type="flex" align="stretch">
			<a-col :span="24" :lg="16">

				<a-row :gutter="24" type="flex" align="stretch">

					<!-- Top widgets -->
					<a-col :span="24" :lg="8" class="mb-24">

						<!-- Line chart widgets -->
						<WidgetLineChart></WidgetLineChart>
						<!-- / Line chart widgets -->

					</a-col>
					<a-col :span="24" :lg="8" class="mb-24">

						<!-- Line chart widgets -->
						<WidgetLineChart></WidgetLineChart>
						<!-- / Line chart widgets -->

					</a-col>
					<a-col :span="24" :lg="8" class="mb-24">

						<a-card class="crm-bar-line header-solid h-full" :bodyStyle="{padding: 0, height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
							<a href="#" class="text-center text-muted font-bold">
								<h4 class="font-semibold text-muted mb-0">+</h4>
								<h6 class="font-semibold text-muted">New tab</h6>
							</a>
						</a-card>

					</a-col>
					<!-- / Top widgets -->

					<!-- Calendar -->
					<a-col :span="24">

						<a-card :bordered="false" class="header-solid mb-24">
							<template #title>
								<h6 class="font-semibold m-0">Calendar</h6>
								<p>{{ date }}</p>
							</template>
							<FullCalendar :options="calendarOptions" />
						</a-card>

					</a-col>
					<!-- / Calendar -->

				</a-row>

			</a-col>
			<a-col :span="24" :lg="8">

				<!-- Information card -->
				<CardInfo2 class="mb-24 h-auto"></CardInfo2>
				<!-- / Information card -->

				<!-- Categories list -->
				<CardCategories class="mb-24 h-auto" :data="categoriesData"></CardCategories>
				<!-- Categories list -->

				<!-- Notification Card -->
				<CardNotification></CardNotification>
				<!-- / Notification Card -->

			</a-col>
			<a-col :span="12" class="mb-24">

				<!-- Your Transactions Card -->
				<CardTransactions2 title="Your Transactions" date="23 - 30 March 2021" :data="transactionsData"></CardTransactions2>
				<!-- / Your Transactions Card -->

			</a-col>
			<a-col :span="12" class="mb-24">

				<!-- Revenue Card -->
				<CardTransactions2 title="Revenue" :data="transactionsData2" date="01 - 07 June 2021"></CardTransactions2>
				<!-- / Revenue Card -->
				
			</a-col>
		</a-row>
		<!-- / Charts -->

	</div>

</template>

<script>

	// Bar chart for Visitors card.
	import WidgetLineChart from '../../components/Widgets/WidgetLineChart' ;

	// Information card 2.
	import CardInfo2 from '../../components/Cards/CardInfo2' ;

	// Notification Card
	import CardNotification from "../../components/Cards/CardNotification"
	
	// Categories List Card
	import CardCategories from '../../components/Cards/CardCategories' ;
	
	// Transactions List Card
	import CardTransactions2 from '../../components/Cards/CardTransactions2' ;

	// FullCalendar and plugins
	import '@fullcalendar/core/vdom' // solves problem with Vite
	import FullCalendar from '@fullcalendar/vue'
	import dayGridPlugin from '@fullcalendar/daygrid'
	import interactionPlugin from '@fullcalendar/interaction'

	const categoriesData = [
		{
			id: "1",
			title: "Devices",
			subtitle: "250 in stock, <span class='font-weight-bold'>346+ sold</span>",
			icon: 'gitlab',
		},
		{
			id: "2",
			title: "Tickets",
			subtitle: "123 closed, <span class='font-weight-bold'>15 open</span>",
			icon: 'audio',
		},
		{
			id: "3",
			title: "Error logs",
			subtitle: "1 is active, <span class='font-weight-bold'>40 closed</span>",
			icon: 'compass',
		},
	] ;

	// "Your Transactions" list data.
	const transactionsData = [
		{
			title: "Netflix",
			datetime: "27 March 2020, at 12:30 PM",
			amount: "2,500",
			type: -1,// 0 is for pending, 1 is for deposit, -1 is for withdrawal.
			status: 'danger',
		},
		{
			title: "Apple",
			datetime: "23 March 2021, at 04:30 AM",
			amount: "2,000",
			type: 1,
			status: 'success',
		},
		{
			title: "Partner #22213",
			datetime: "19 March 2021, at 12:30 AM",
			amount: "750",
			type: 1,
			status: 'success',
		},
	] ;

	// "Your Transactions" list data.
	const transactionsData2 = [
		{
			title: "via PayPal",
			datetime: "07 June 2021, at 09:00 AM",
			amount: "4,999",
			type: 1,
			status: 'success',
		},
		{
			title: "Partner #90211",
			datetime: "07 June 2021, at 05:50 AM",
			amount: "700",
			type: 1,
			status: 'success',
		},
		{
			title: "Services",
			datetime: "07 June 2021, at 07:10 PM",
			amount: "1,800",
			type: 1,
			status: 'success',
		},
	] ;
	

	export default ({
		components: {
			FullCalendar, // make the <FullCalendar> tag available
			WidgetLineChart,
			CardInfo2,
			CardNotification,
			CardCategories,
			CardTransactions2,
		},
		data() {
			return {
				transactionsData,
				transactionsData2,
				categoriesData,
				calendarOptions: {
					plugins: [ dayGridPlugin, interactionPlugin ],
					contentHeight: 'auto',
					initialView: 'dayGridMonth',
					selectable: true,
					initialDate: '2020-12-01',
					editable: true,
					headerToolbar: false,
					events: [{
							title: 'Call with Dave',
							start: '2020-11-18',
							end: '2020-11-18',
							className: 'bg-danger'
						},

						{
							title: 'Lunch meeting',
							start: '2020-11-21',
							end: '2020-11-22',
							className: 'bg-warning text-dark'
						},

						{
							title: 'All day conference',
							start: '2020-11-29',
							end: '2020-11-29',
							className: 'bg-success'
						},

						{
							title: 'Meeting with Mary',
							start: '2020-12-01',
							end: '2020-12-01',
							className: 'bg-primary'
						},

						{
							title: 'Winter Hackaton',
							start: '2020-12-03',
							end: '2020-12-03',
							className: 'bg-danger'
						},

						{
							title: 'Digital event',
							start: '2020-12-07',
							end: '2020-12-09',
							className: 'bg-warning text-dark'
						},

						{
							title: 'Marketing event',
							start: '2020-12-10',
							end: '2020-12-10',
							className: 'bg-primary'
						},

						{
							title: 'Dinner with Family',
							start: '2020-12-19',
							end: '2020-12-19',
							className: 'bg-danger'
						},

						{
							title: 'Black Friday',
							start: '2020-12-23',
							end: '2020-12-23',
							className: 'bg-primary'
						},

						{
							title: 'Cyber Week',
							start: '2020-12-02',
							end: '2020-12-02',
							className: 'bg-warning text-dark'
						},
					]
				}
			}
		},
		computed: {
			// Current date for calendar.
			date(){
				var today = new Date();
				var mYear = today.getFullYear();
				var weekday = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
				var mDay = weekday[today.getDay()];
				
				return `${mDay}, ${mYear}` ;
			}
		},
	})

</script>

<style lang="scss">
</style>