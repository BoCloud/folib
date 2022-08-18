<template>
	<div>
		<!-- Radar chart -->
		<canvas ref="chart" class="chart-radar" :style="{'height': height + 'px'}"></canvas>
		<!-- / Radar chart -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: [
		],
		data(){
			return {
				height: 500,
			} ;
		},
		mounted () { 
    		let ctx = this.$refs.chart.getContext("2d");

			this.chart = new Chart(ctx, {
				type: "radar",
				data: {
					labels: ["English", "Maths", "Physics", "Chemistry", "Biology", "History"],
					datasets: [{
					label: "Student A",
					backgroundColor: "rgba(20,20,20,0.15)",
					data: [65, 75, 70, 80, 60, 80],
					borderDash: [5, 5],
					}, {
					label: "Student B",
					backgroundColor: "rgba(24, 144, 255, .2)",
					data: [54, 65, 60, 70, 70, 75]
					}]
				},
				options: {
					plugins: {
					legend: {
						display: false,
					}
					}
				}
			});
		},
		// Right before the component is destroyed,
		// also destroy the chart.
		beforeDestroy: function () {
			this.chart.destroy() ;
		},
	})

</script>

<style lang="scss" scoped>
</style>