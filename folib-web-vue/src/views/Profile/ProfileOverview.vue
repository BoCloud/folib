<!-- 
	This is the profile overview page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
	<div>

		<!-- Header Background Image -->
		<div class="profile-nav-bg" style="background-image: url('images/bg-profile.jpg')"></div>
		<!-- / Header Background Image -->

		<!-- User Profile Card -->
		<a-card :bordered="false" class="card-profile-head" :bodyStyle="{padding: 0,}">
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

		<a-row type="flex" :gutter="24">

			<!-- Platform Settings Column -->
			<a-col :span="24" :md="8" class="mb-24">

				<!-- Platform Settings Card -->
				<CardPlatformSettings></CardPlatformSettings>
				<!-- / Platform Settings Card -->

			</a-col>
			<!-- / Platform Settings Column -->

			<!-- Profile Information Column -->
			<a-col :span="24" :md="8" class="mb-24">

				<!-- Profile Information Card -->
				<CardProfileInformation></CardProfileInformation>
				<!-- / Profile Information Card -->

			</a-col>
			<!-- / Profile Information Column -->
			
			<!-- Conversations Column -->
			<a-col :span="24" :md="8" class="mb-24">
			
				<!-- Conversations Card -->
				<CardConversations
					:data="conversationsData"
				></CardConversations>
				<!-- / Conversations Card -->

			</a-col>
			<!-- / Conversations Column -->

		</a-row>
		
		<!-- Projects Card -->
		<a-card :bordered="false" class="header-solid h-full mb-24" :bodyStyle="{paddingTop: '14px'}">
			<template #title>
				<h6 class="font-semibold">Projects</h6>			
				<p>Architects design houses</p>	
			</template>

			<a-row type="flex" :gutter="[24,24]" align="stretch">

				<!-- Project Column -->
				<a-col :span="24" :md="12" :xl="6" v-for="(project, index) in projects" :key="index">
					<CardProject
						:id="project.id"
						:title="project.title"
						:content="project.content"
						:cover="project.cover"
						:team="project.team"
					></CardProject>
				</a-col>
				<!-- / Project Column -->

				<!-- Project Column -->
				<a-col :span="24" :md="12" :xl="6">

					<!-- Project Upload Component -->
					<a-upload
						name="avatar"
						list-type="picture-card"
						class="projects-uploader"
						:show-upload-list="false"
					>
						<img v-if="false" src="" alt="avatar" />
						<div v-else>
							<a-icon v-if="false" type="loading" />
							<svg v-else width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
								<path fill-rule="evenodd" clip-rule="evenodd" d="M3 17C3 16.4477 3.44772 16 4 16H16C16.5523 16 17 16.4477 17 17C17 17.5523 16.5523 18 16 18H4C3.44772 18 3 17.5523 3 17ZM6.29289 6.70711C5.90237 6.31658 5.90237 5.68342 6.29289 5.29289L9.29289 2.29289C9.48043 2.10536 9.73478 2 10 2C10.2652 2 10.5196 2.10536 10.7071 2.29289L13.7071 5.29289C14.0976 5.68342 14.0976 6.31658 13.7071 6.70711C13.3166 7.09763 12.6834 7.09763 12.2929 6.70711L11 5.41421L11 13C11 13.5523 10.5523 14 10 14C9.44771 14 9 13.5523 9 13L9 5.41421L7.70711 6.70711C7.31658 7.09763 6.68342 7.09763 6.29289 6.70711Z" fill="#111827"/>
							</svg>

							<div class="ant-upload-text font-semibold text-dark">
								Upload New Project
							</div>
						</div>
					</a-upload>
					<!-- / Project Upload Component -->

				</a-col>
				<!-- / Project Column -->

			</a-row>
		</a-card>
		<!-- / Projects Card -->

	</div>
</template>

<script>

	import CardPlatformSettings from "../../components/Cards/CardPlatformSettings"
	import CardProfileInformation from "../../components/Cards/CardProfileInformation"
	import CardConversations from "../../components/Cards/CardConversations"
	import CardProject from "../../components/Cards/CardProject"

	// Conversation's list data.
	const conversationsData = [
		{
			id: "1",
			title: "Sophie B.",
			code: "Hi! I need more information…",
			avatar: "images/face-3.jpg",
		},
		{
			id: "2",
			title: "Anne Marie",
			code: "Awesome work, can you…",
			avatar: "images/face-4.jpg",
		},
		{
			id: "3",
			title: "Ivan",
			code: "About files I can…",
			avatar: "images/face-5.jpeg",
		},
		{
			id: "4",
			title: "Peterson",
			code: "Have a great afternoon…",
			avatar: "images/face-6.jpeg",
		},
		{
			id: "5",
			title: "Nick Daniel",
			code: "Hi! I need more information…",
			avatar: "images/face-2.jpg",
		},
	] ;

	// Project cards data
	const projects = [
		{
			id: 1,
			title: "Modern",
			content: "As Uber works through a huge amount of internal management turmoil.",
			cover: "images/home-decor-3.jpeg",
			team: [
				"images/face-1.jpg",
				"images/face-4.jpg",
				"images/face-2.jpg",
				"images/face-3.jpg",
			],
		},
		{
			id: 2,
			title: "Scandinavian",
			content: "Music is something that every person has his or her own specific opinion about.",
			cover: "images/home-decor-2.jpeg",
			team: [
				"images/face-1.jpg",
				"images/face-4.jpg",
				"images/face-2.jpg",
				"images/face-3.jpg",
			],
		},
		{
			id: 3,
			title: "Minimalist",
			content: "Different people have different taste, and various types of music, Zimbali Resort.",
			cover: "images/home-decor-1.jpeg",
			team: [
				"images/face-1.jpg",
				"images/face-4.jpg",
				"images/face-2.jpg",
				"images/face-3.jpg",
			],
		},
	] ;

	export default ({
		components: {
			CardPlatformSettings,
			CardProfileInformation,
			CardConversations,
			CardProject,
		},
		data() {
			return {
				// Active button for the "User Profile" card's radio button group.
				profileHeaderBtns: 'overview',

				// Associating Conversation's list data with its corresponding property.
				conversationsData,

				// Project cards data
				projects,
			}
		},
	})

</script>

<style lang="scss">
</style>