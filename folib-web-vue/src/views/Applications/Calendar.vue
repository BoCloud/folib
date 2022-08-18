<!-- 
	This is the Calendar page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<!-- Calendar header -->
		<div class="calendar-head my-15">

			<div class="ml-auto">
				<h6 class="text-sm text-muted">Team members :</h6>
				<a-space :size="-12" class="avatar-chips">
					<a-avatar :size="36" src="images/face-1.jpg" />
					<a-avatar :size="36" src="images/face-4.jpg" />
					<a-avatar :size="36" src="images/face-2.jpg" />
					<a-avatar :size="36" src="images/face-3.jpg" />
					<a-avatar :size="36" src="images/face-5.jpeg" />
				</a-space>
			</div>
			<div>
				<a-button type="primary">
					<a-icon type="plus" class="m-0" />
				</a-button>
			</div>

		</div>
		<!-- / Calendar header -->
		
		<!-- Charts -->
		<a-row :gutter="24" type="flex">

			<!-- Calendar column -->
			<a-col :span="24" :lg="18" class="mb-24">
				<a-card class="card-calendar">
 					<FullCalendar :options="calendarOptions" />
				</a-card>
			</a-col>
			<!-- / Calendar column -->
			
			<!-- Events Column -->
			<a-col :span="24" :md="6">
			
				<!-- Next Event card -->
				<CardNextEvents
					:data="eventsData" class="mb-24"
				></CardNextEvents>
				<!-- / Next Event card -->


				<!-- Productivity chart card -->
				<a-card :bordered="false" class="card-chart-full header-solid mb-24" :bodyStyle="{padding: 0,}">
					<template #title>
						<h6 class="font-semibold m-0">Productivity</h6>
						<p>
							<a-icon type="arrow-up" class="text-success mr-5" />
							<strong>4% more</strong> in 2021
						</p>
					</template>
					<template slot="extra" class="mb-0">
						<a-dropdown :trigger="['click']">
							<a class="ant-dropdown-link p-10" @click="e => e.preventDefault()">
								<a-icon type="ellipsis" class="text-white" />
							</a>
							<a-menu slot="overlay">
								<a-menu-item key="0">
									<a href="http://www.alipay.com/">1st menu item</a>
								</a-menu-item>
								<a-menu-item key="1">
									<a href="http://www.taobao.com/">2nd menu item</a>
								</a-menu-item>
								<a-menu-divider />
								<a-menu-item key="3">
									3rd menu item
								</a-menu-item>
							</a-menu>
						</a-dropdown>
					</template>
					<ChartLineFull :data="chartData" :height="100"></ChartLineFull>
				</a-card>
				<!-- / Productivity chart card -->

			</a-col>
			<!-- / Events Column -->

		</a-row>
		<!-- / Charts -->

	</div>

</template>

<script>
	import '@fullcalendar/core/vdom' // solves problem with Vite
	import FullCalendar from '@fullcalendar/vue'
	import dayGridPlugin from '@fullcalendar/daygrid'
	import interactionPlugin from '@fullcalendar/interaction'
	import CardNextEvents from "../../components/Cards/CardNextEvents"
	import ChartLineFull from "../../components/Charts/ChartLineFull"

	// Next event's list.
	const eventsData = [
		{
			id: "1",
			title: "Cyber Week",
			code: "27 March 2021, at 12:30 PM",
			iconClass: "text-danger",
			icon: "calendar",
			iconBgColor: "rgba(234,6,6,.03)",
		},
		{
			id: "2",
			title: "Meeting with Marry",
			code: "24 March 2021, at 10:00 PM",
			iconClass: "text-primary",
			icon: "bell",
			iconBgColor: "rgba(121,40,202,.03)",
		},
		{
			id: "3",
			title: "Book Deposit Hall",
			code: "25 March 2021, at 9:30 AM",
			iconClass: "text-success",
			icon: "book",
			iconBgColor: "rgba(23,173,55,.03)",
		},
		{
			id: "4",
			title: "Shipment Deal UK",
			code: "25 March 2021, at 2:00 PM",
			iconClass: "text-warning",
			icon: "car",
			iconBgColor: "rgba(245,57,57,.03)",
		},
		{
			id: "5",
			title: "Verify Dashboard Color Palette",
			code: "26 March 2021, at 9:00 AM",
			iconClass: "text-primary",
			icon: "windows",
			iconBgColor: "rgba(33,82,255,.03)",
		},
	] ;

	// Chart data.
	const chartData = {
		labels: ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
		datasets: [{
			label: "Visitors",
			tension: 0.5,
			borderWidth: 0,
			pointRadius: 0,
			borderColor: "#fff",
			borderWidth: 2,
			data: [50, 45, 60, 60, 80, 65, 90, 80, 100],
			maxBarThickness: 6,
			fill: true
		}],
	} ;

	export default {
		components: {
			FullCalendar,
			CardNextEvents,
			ChartLineFull,
		},
		data() {
			return {

				// Next event's list.
				eventsData,

				// Chart data.
				chartData,

				// Calendar options.
				calendarOptions: {
					plugins: [ dayGridPlugin, interactionPlugin ],
					initialView: 'dayGridMonth',
					contentHeight: 'auto',
					initialView: "dayGridMonth",
					headerToolbar: {
						start: 'title', // will normally be on the left. if RTL, will be on the right
						center: '',
						end: 'today prev,next' // will normally be on the right. if RTL, will be on the left
					},
					selectable: true,
					editable: true,
					initialDate: '2020-12-01',
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
					],
					views: {
						month: {
							titleFormat: {
								month: "long",
								year: "numeric"
							}
						},
						agendaWeek: {
							titleFormat: {
								month: "long",
								year: "numeric",
								day: "numeric"
							}
						},
						agendaDay: {
							titleFormat: {
								month: "short",
								year: "numeric",
								day: "numeric"
							}
						}
					},
				}

			}
		}
	}

</script>

<style lang="scss">
</style>