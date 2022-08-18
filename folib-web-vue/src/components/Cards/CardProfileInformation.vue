<template>

	<!-- Profile Information Card -->
	<a-card :bordered="false" class="header-solid h-full card-profile-information" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }" :headStyle="{paddingRight: 0,}">
		<template #title>
			<h6 class="font-semibold m-0">Profile Information</h6>
		</template>
		<a-button type="link" slot="extra">
			<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
				<path class="fill-muted" d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z" fill="#111827"/>
				<path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827"/>
			</svg>
		</a-button>
		<p class="text-dark">
			Hi, I’m Alec Thompson, Decisions: If you can’t decide, the answer is no. If two equally difficult paths, choose the one more painful in the short term (pain avoidance is creating an illusion of equality).
		</p>
		<hr class="my-25">
		<a-descriptions title="Oliver Liam" :column="1">
			<a-descriptions-item label="Full Name">
				Sarah Emily Jacob
			</a-descriptions-item>
			<a-descriptions-item label="Mobile">
				(44) 123 1234 123
			</a-descriptions-item>
			<a-descriptions-item label="Email">
				sarahjacob@mail.com
			</a-descriptions-item>
			<a-descriptions-item label="Location">
				USA
			</a-descriptions-item>
			<a-descriptions-item label="Social">
				<a href="#" class="mx-5 px-5">
					<a-icon type="twitter" style="color: #3EA1EC;" />
				</a>
				<a href="#" class="mx-5 px-5">
					<a-icon type="facebook" style="color: #344E86" />
				</a>
				<a href="#" class="mx-5 px-5">
					<a-icon type="instagram" style="color: #E1306C" />
				</a>
			</a-descriptions-item>
		</a-descriptions>
	</a-card>
	<!-- / Profile Information Card -->

</template>

<script>

	export default ({
		data() {
			return {
			}
		},
	})

</script>