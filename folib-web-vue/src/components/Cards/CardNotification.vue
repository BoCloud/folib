<template>

	<!-- Notification Card -->
	<a-card :bordered="false" class="card-notification header-solid">
		<a-row type="flex" align="middle">
			<a-col :span="8">
				<a-avatar
					:size="100"
					shape="square"
					src="images/kal-visuals-square.jpg"
				/>
			</a-col>
			<a-col :span="16">
				<p class="font-semibold">Today is Martina's birthday. Wish her the best of luck!</p>
				<a-button type="primary" size="small">Send message</a-button>
			</a-col>
		</a-row>
	</a-card>
	<!-- / Notification Card -->

</template>

<script>

	export default ({
		data() {
			return {
			}
		},
	})

</script>