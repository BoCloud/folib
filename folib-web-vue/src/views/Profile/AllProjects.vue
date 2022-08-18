<!-- 
	This is the All projects page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
	<div>

		<!-- Header Background Image -->
		<div class="profile-nav-bg" style="background-image: url('images/bg-profile.jpg')"></div>
		<!-- / Header Background Image -->

		<!-- User Profile Card -->
		<a-card :bordered="false" class="card-profile-head">
			<template #title>
				<a-row type="flex" align="middle">
					<a-col :span="24" :md="12" class="col-info">
						<a-avatar :size="74" shape="square" src="images/face-1.jpg" />
						<div class="avatar-info">
							<h4 class="font-semibold m-0">Sarah Jacob</h4>
							<p>CEO / Co-Founder</p>
						</div>
					</a-col>
					<a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
						<a-radio-group v-model="profileHeaderBtns" size="small">
							<a-radio-button value="overview">OVERVIEW</a-radio-button>
							<a-radio-button value="teams">TEAMS</a-radio-button>
							<a-radio-button value="projects">PROJECTS</a-radio-button>
						</a-radio-group>
					</a-col>
				</a-row>
			</template>
		</a-card>
		<!-- User Profile Card -->

		<a-row type="flex" :gutter="[24,24]">
			<a-col :span="16">
				<h5 class="font-semibold">Some of Our Awesome Projects</h5>
				<p class="text-lg">
					This is the paragraph where you can write more details about your projects.
					Keep you user engaged by providing meaningful information.
				</p>
			</a-col>
		</a-row>
		
		<!-- Project Cards -->
		<a-row type="flex" :gutter="24">
			<a-col :span="8" class="mb-24">
				<!-- Project Card -->
				<CardProject2
					title="Slack Bot"
					logo="images/logos/logo-slack.svg"
					:team="['images/face-1.jpg', 'images/face-2.jpg', 'images/face-3.jpg', 'images/face-4.jpg', 'images/face-5.jpeg', ]"
					participants="5"
					due="02.03.22"
				>
					If everything I did failed - which it doesn't, I think that it actually succeeds.
				</CardProject2>
				<!-- / Project Card -->
			</a-col>
			<a-col :span="8" class="mb-24">
				<!-- Project Card -->
				<CardProject2
					title="Premium support"
					logo="images/logos/logo-spotify.svg"
					:team="['images/face-1.jpg', 'images/face-2.jpg', 'images/face-3.jpg', 'images/face-4.jpg', 'images/face-5.jpeg', ]"
					participants="3"
					due="22.11.21"
				>
					Pink is obviously a better color. Everyone’s born confident, and everything’s taken away from you.
				</CardProject2>
				<!-- / Project Card -->
			</a-col>
			<a-col :span="8" class="mb-24">
				<!-- Project Card -->
				<CardProject2
					title="Design tools"
					logo="images/small-logos/logo-xd.svg"
					:team="['images/face-1.jpg', 'images/face-2.jpg', 'images/face-3.jpg', 'images/face-4.jpg', 'images/face-5.jpeg', ]"
					participants="4"
					due="06.03.20"
				>
					Constantly growing. We’re constantly making mistakes from which we learn and improve.
				</CardProject2>
				<!-- / Project Card -->
			</a-col>
			<a-col :span="8" class="mb-24">
				<!-- Project Card -->
				<CardProject2
					title="Looking great"
					logo="images/logos/logo-asana.svg"
					:team="['images/face-1.jpg', 'images/face-2.jpg', 'images/face-3.jpg', 'images/face-4.jpg', 'images/face-5.jpeg', ]"
					participants="6"
					due="14.03.24"
				>
					You have the opportunity to play this game of life you need to appreciate every moment.
				</CardProject2>
				<!-- / Project Card -->
			</a-col>
			<a-col :span="8" class="mb-24">
				<!-- Project Card -->
				<CardProject2
					title="Developer First"
					logo="images/logos/logo-invision.svg"
					:team="['images/face-1.jpg', 'images/face-2.jpg', 'images/face-3.jpg', 'images/face-4.jpg', 'images/face-5.jpeg', ]"
					participants="4"
					due="16.01.22"
				>
					For standing out. But the time is now to be okay to be the greatest you.
				</CardProject2>
				<!-- / Project Card -->
			</a-col>
			<a-col :span="8" class="mb-24">

				<a-card class="crm-bar-line header-solid h-full" :bodyStyle="{padding: 0, height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
					<a href="#" class="text-center text-muted font-bold">
						<h3 class="font-semibold text-muted mb-0">+</h3>
						<h5 class="font-semibold text-muted">New project</h5>
					</a>
				</a-card>

			</a-col>
		</a-row>
		<!-- / Project Cards -->

	</div>
</template>

<script>

	import CardProject2 from "../../components/Cards/CardProject2"

	export default ({
		components: {
			CardProject2,
		},
		data() {
			return {
				// Active button for the "User Profile" card's radio button group.
				profileHeaderBtns: 'overview',
			}
		},
	})

</script>

<style lang="scss">
</style>