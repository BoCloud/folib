<!-- 
	This is the Timeline page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<a-row type="flex" :gutter="[24,24]">

			<a-col :span="24" :lg="12">
				
				<!-- Orders History Timeline Card -->
				<a-card :bordered="false" class="header-solid mb-24" :bodyStyle="{paddingTop: '12px',}">
					<template #title>
						<h6>Timeline with dotted line</h6>			
					</template>
					<a-timeline>
						<a-timeline-item color="green">
							$2400, Design changes
							<small>22 DEC 7:20 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="danger">
							New order #1832412
							<small>21 DEC 11 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="primary">
							Server payments for April
							<small>21 DEC 9:34 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="warning">
							New card added for order #4395133
							<small>20 DEC 2:20 AM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="secondary">
							Unlock packages for development
							<small>18 DEC 4:54 AM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="success">
							New message unread
							<small>16 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="green">
							Notifications unread
							<small>15 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="warning">
							New request
							<small>14 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="dark">
							Controller issues
							<small>13 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
					</a-timeline>
				</a-card>
				<!-- / Orders History Timeline Card -->
				
			</a-col>

			<a-col :span="24" :lg="12">
				
				<!-- Orders History Timeline Card -->
				<a-card :bordered="false" class="bg-gray-10 header-solid mb-24" :bodyStyle="{paddingTop: '12px',}">
					<template #title>
						<h6 class="text-white">Timeline dark with dashed line</h6>			
					</template>
					<a-timeline class="timeline-dark">
						<a-timeline-item color="green">
							$2400, Design changes
							<small>22 DEC 7:20 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="danger">
							New order #1832412
							<small>21 DEC 11 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="primary">
							Server payments for April
							<small>21 DEC 9:34 PM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="warning">
							New card added for order #4395133
							<small>20 DEC 2:20 AM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="secondary">
							Unlock packages for development
							<small>18 DEC 4:54 AM</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="success">
							New message unread
							<small>16 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="green">
							Notifications unread
							<small>15 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="warning">
							New request
							<small>14 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
						<a-timeline-item color="white">
							Controller issues
							<small>13 DEC</small>
							<p>
								People care about how you see the world, how you think, what motivates you, what you’re struggling with or afraid of.
							</p>
						</a-timeline-item>
					</a-timeline>
				</a-card>
				<!-- / Orders History Timeline Card -->
				
			</a-col>

		</a-row>

	</div>

</template>

<script>

	export default {
		data() {
			return {
			};
		},
		methods: {
		},
	};
</script>

<style lang="scss">
</style>