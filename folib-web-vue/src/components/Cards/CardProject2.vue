<template>

	<!-- Project 2 Card -->
	<a-card :bordered="false" class="card-project-2 header-solid" :bodyStyle="{paddingTop: '14px' }" :headStyle="{paddingBottom: '0' }">
		<template #title>
			<a-row type="flex" :gutter="[24,24]" align="middle">
				<a-col>
					<a-avatar
						:size="74"
						shape="square"
						style="border-radius: 8px; background-image: linear-gradient( 310deg, #141727, #3a416f );"
					>
						<img :src="logo" style="width: 100%;" alt="">
					</a-avatar>
				</a-col>
				<a-col>
					<h6 class="font-semibold mb-10">{{ title }}</h6>
					<a-space :size="-12" class="avatar-chips">
						<a-avatar size="small" v-for="(src, key) in team" :key="key" :src="src" />
					</a-space>
				</a-col>
				<a-col class="ml-auto pt-10" style="align-self: flex-start;">
					<a href="#">
						<a-icon type="more" class="text-muted" style="font-size: 18px;" />
					</a>
				</a-col>
			</a-row>
		</template>
		<p><slot></slot></p>
		<hr>
		<a-row type="flex" :gutter="[24,24]" align="middle">
			<a-col>
				<h6 class="font-semibold text-md mb-0">{{ participants }}</h6>
				<p class="font-semibold text-muted mb-0">Participants</p>
			</a-col>
			<a-col class="ml-auto">
				<h6 class="font-semibold text-md mb-0">{{ due }}</h6>
				<p class="font-semibold text-muted mb-0">Due date</p>
			</a-col>
		</a-row>
	</a-card>
	<!-- / Project 2 Card -->

</template>

<script>

	export default ({
		props: {
			title: {
				type: String,
				default: "",
			},
			logo: {
				type: String,
				default: "",
			},
			team: {
				type: Array,
				default: () => [],
			},
			participants: {
				type: [Number, String],
				default: 0,
			},
			due: {
				type: String,
				default: "",
			},
		},
		data() {
			return {
			}
		},
	})

</script>