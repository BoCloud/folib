<template>

	<!-- Line Chart widget -->
	<a-card :bordered="false" class="crm-bar-line header-solid" :bodyStyle="{padding: 0,}">
		<template #title>
			<h6 class="text-md font-semibold text-muted">Visitors</h6>			
			<h5 class="mb-0"> 5,927 <small class="text-success">+20%</small></h5>	
		</template>
		<chart-line :height="100" :data="lineChartData"></chart-line>
	</a-card>
	<!-- Line Chart widget -->

</template>

<script>

	// Bar chart for "Active Users" card.
	import ChartLine from '../Charts/ChartLineWidget' ;

	export default ({
		components: {
			ChartLine,
		},
		data() {
			return {

				// Data for line chart.
				lineChartData: {
					labels: ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
					datasets: [{
						label: "Visitors",
						tension: 0.5,
						borderWidth: 0,
						pointRadius: 0,
						borderColor: "#cb0c9f",
						borderWidth: 2,
						data: [50, 45, 60, 60, 80, 65, 90, 80, 100],
						maxBarThickness: 6,
						fill: true
					}],
				},
			}
		},
	})

</script>