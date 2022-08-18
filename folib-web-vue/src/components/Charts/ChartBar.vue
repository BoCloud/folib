<template>
	<div>
		<!-- Bar chart -->
		<canvas ref="chart" class="chart-bar" :style="{'height': height + 'px'}"></canvas>
		<!-- / Bar chart -->
	</div>
</template>

<script>
	import { Chart, registerables } from 'chart.js';
	Chart.register(...registerables);

	export default ({
		props: [
			'data',
			'height',
		],
		data(){
			return {
				chart: null,
			} ;
		},
		mounted () { 
       this.buildData()
		},
    methods:{
      buildData(){
        let ctx = this.$refs.chart.getContext("2d");
        if(this.chart){
          this.chart.destroy() ;
        }
        this.chart = new Chart(ctx, {
          type: "bar",
          data: this.data,
          options: {
            layout: {
              padding: {
                top: 30,
                right: 15,
                left: 10,
                bottom: 5,
              },
            },
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
              legend: {
                display: false,
              },
            },
            tooltips: {
              enabled: true,
              mode: "index",
              intersect: false,
            },
            scales: {
              y: {
                grid: {
                  display: true,
                  color: "rgba(255, 255, 255, .2)",
                  zeroLineColor: "#ffffff",
                  borderDash: [6],
                  borderDashOffset: [6],
                },
                ticks: {
                  suggestedMin: 0,
                  suggestedMax: 1000,
                  display: true,
                  color: "#fff",
                  font: {
                    size: 14,
                    lineHeight: 1.5,
                    weight: '600',
                    family: "Open Sans",
                  },
                },
              },
              x: {
                grid: {
                  display: false,
                },
                ticks: {
                  display: true,
                  color: "#fff",
                  font: {
                    size: 14,
                    lineHeight: 1.5,
                    weight: '600',
                    family: "Open Sans",
                  },
                },
              },
            },
          }
        }) ;
      }
    },
		// Right before the component is destroyed,
		// also destroy the chart.
		beforeDestroy: function () {
			this.chart.destroy() ;
		},
	})

</script>

<style lang="scss" scoped>
	canvas {
		background-image: linear-gradient(to right, #00369E, #005CFD, #A18DFF ) ;
	}
</style>