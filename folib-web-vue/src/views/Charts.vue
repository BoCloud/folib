<!-- 
	This is the Charts page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
	<div>

		<!-- Charts page head -->
		<a-row :gutter="24" type="flex" align="stretch">
			<a-col :span="24" :lg="12" class="mb-24">
				<h5 class="font-semibold">Charts</h5>
				<p>
					Charts on this page use Chart.js - Simple yet flexible JavaScript charting for designers &amp; developers.
				</p>

			</a-col>
		</a-row>
		<!-- / Charts page head -->

		<!-- Charts -->
		<a-row :gutter="24" type="flex" align="stretch">
			
			<!-- Line chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Line chart</h6>			
					</template>
					<ChartLine2></ChartLine2>
				</a-card>
			</a-col>
			<!-- / Line chart -->
			
			<!-- Line chart with gradient -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Line chart with gradient</h6>			
					</template>
					<ChartLineGradient></ChartLineGradient>
				</a-card>
			</a-col>
			<!-- / Line chart with gradient -->
			
			<!-- Bar chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Bar chart</h6>			
					</template>
					<ChartBar2></ChartBar2>
				</a-card>
			</a-col>
			<!-- / Bar chart -->
			
			<!-- Bar chart horizontal -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Bar chart horizontal</h6>			
					</template>
					<ChartBarHorizontal></ChartBarHorizontal>
				</a-card>
			</a-col>
			<!-- / Bar chart horizontal -->
			
			<!-- Mixed chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Mixed chart</h6>			
					</template>
					<ChartMixed></ChartMixed>
				</a-card>
			</a-col>
			<!-- / Mixed chart -->
			
			<!-- Bubble chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 12px 8px 3px'}">
					<template #title>
						<h6>Bubble chart</h6>			
					</template>
					<ChartBubble></ChartBubble>
				</a-card>
			</a-col>
			<!-- / Bubble chart -->
			
			<!-- Doughnut chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 40px 40px'}">
					<template #title>
						<h6>Doughnut chart</h6>			
					</template>
					<ChartDoughnut></ChartDoughnut>
				</a-card>
			</a-col>
			<!-- / Doughnut chart -->
			
			<!-- Pie chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 40px 40px'}">
					<template #title>
						<h6>Pie chart</h6>			
					</template>
					<ChartPie></ChartPie>
				</a-card>
			</a-col>
			<!-- / Pie chart -->
			
			<!-- Radar chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 40px 40px'}">
					<template #title>
						<h6>Radar chart</h6>			
					</template>
					<ChartRadar></ChartRadar>
				</a-card>
			</a-col>
			<!-- / Radar chart -->
			
			<!-- Polar chart -->
			<a-col :span="24" :lg="12" class="mb-24">
				<a-card :bordered="false" class="header-solid" :bodyStyle="{padding: '0 40px 40px'}">
					<template #title>
						<h6>Polar chart</h6>			
					</template>
					<ChartPolar></ChartPolar>
				</a-card>
			</a-col>
			<!-- / Polar chart -->
			
		</a-row>
		<!-- / Charts -->

	</div>
</template>

<script>

	// Importing charts
	import ChartLine2 from '../components/Charts/ChartLine2' ;
	import ChartLineGradient from '../components/Charts/ChartLineGradient' ;
	import ChartBar2 from '../components/Charts/ChartBar2' ;
	import ChartBarHorizontal from '../components/Charts/ChartBarHorizontal' ;
	import ChartMixed from '../components/Charts/ChartMixed' ;
	import ChartBubble from '../components/Charts/ChartBubble' ;
	import ChartDoughnut from '../components/Charts/ChartDoughnut' ;
	import ChartPie from '../components/Charts/ChartPie' ;
	import ChartRadar from '../components/Charts/ChartRadar' ;
	import ChartPolar from '../components/Charts/ChartPolar' ;

	export default ({
		components: {
			ChartLine2,
			ChartLineGradient,
			ChartBar2,
			ChartBarHorizontal,
			ChartMixed,
			ChartBubble,
			ChartDoughnut,
			ChartPie,
			ChartRadar,
			ChartPolar,
		},
		data() {
			return {
			}
		},
	})

</script>

<style lang="scss">
</style>