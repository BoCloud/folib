<template>

	<!-- Platform Settings Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
		<template #title>
			<h6 class="font-semibold m-0">Platform Settings</h6>
		</template>
		<ul class="list settings-list">
			<li>
				<h6 class="list-header text-sm text-muted">ACCOUNT</h6>
			</li>
			<li>
				<a-switch v-model="emailForFollows" />
				<span>Email me when someone follows me</span>
			</li>
			<li>
				<a-switch v-model="emailForAnswers"/>
				<span>Email me when someone answers me</span>
			</li>
			<li>
				<a-switch v-model="emailForMentions" />
				<span>Email me when someone mentions me</span>
			</li>
			<li>
				<h6 class="list-header text-sm text-muted m-0">APPLICATION</h6>
			</li>
			<li>
				<a-switch v-model="emailForNewProjects" />
				<span>New launches and projects</span>
			</li>
			<li>
				<a-switch v-model="emailForProductUpdates" />
				<span>Monthly product updates</span>
			</li>
			<li>
				<a-switch v-model="emailForNewsletter" />
				<span>Subscribe to newsletter</span>
			</li>
		</ul>
	</a-card>
	<!-- / Platform Settings Card -->

</template>

<script>

	export default ({
		data() {
			return {
				// Binded model property for "Platform Settings" switch button for "Email ... follows me" .
				emailForFollows: true,

				// Binded model property for "Platform Settings" switch button for "Email ... answers me" .
				emailForAnswers: false,

				// Binded model property for "Platform Settings" switch button for "Email ... mentions me" .
				emailForMentions: true,

				// Binded model property for "Platform Settings" switch button for "New launches and projects" .
				emailForNewProjects: true,

				// Binded model property for "Platform Settings" switch button for "Monthly product updates" .
				emailForProductUpdates: false,

				// Binded model property for "Platform Settings" switch button for "Subscribe to newsletter" .
				emailForNewsletter: true,
			}
		},
	})

</script>
