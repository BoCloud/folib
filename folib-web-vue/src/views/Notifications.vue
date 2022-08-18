<!-- 
	This is the Notifications page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
	<div>

		<a-row :gutter="24" type="flex" align="stretch">
			
			<!-- Default Notifications -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Notifications</h6>			
					</template>

					<a-row type="flex" :gutter="[24,24]">
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openBasicNotification">
								Basic
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomizedIconNotification">
								Customized Icon
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomizedStyleNotification">
								Customized style
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomDurationNotification">
								Custom duration
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomCloseButtonNotification()">
								Custom close button
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openUpdateMessageContentNotification()">
								Update Message Content
							</a-button>
						</a-col>

					</a-row>

				</a-card>
			</a-col>
			<!-- / Default Notifications -->

			<!-- Various Positions -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Positions</h6>			
					</template>

					<a-row type="flex" :gutter="[24,24]">
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomPositionedNotification('topLeft')">
								<a-icon type="radius-upleft" />
								Top Left
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomPositionedNotification('topRight')">
								<a-icon type="radius-upright" />
								Top Right
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomPositionedNotification('bottomLeft')">
								<a-icon type="radius-bottomleft" />
								Bottom Left
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openCustomPositionedNotification('bottomRight')">
								<a-icon type="radius-bottomright" />
								Bottom Right
							</a-button>
						</a-col>

					</a-row>

				</a-card>
			</a-col>
			<!-- / Various Positions -->
			
			<!-- Various States -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Various States</h6>			
					</template>

					<a-row type="flex" :gutter="[24,24]">
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-success text-white" block @click="openNotificationWithIcon('success')">
								Success
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-info text-white" block @click="openNotificationWithIcon('info')">
								Info
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-warning" block @click="openNotificationWithIcon('warning')">
								Warning
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="danger" block @click="openNotificationWithIcon('error')">
								Error
							</a-button>
						</a-col>

					</a-row>

				</a-card>
			</a-col>
			<!-- / Various States -->
			
			<!-- Solid State Notifications -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Solid States</h6>			
					</template>

					<a-row type="flex" :gutter="[24,24]">
						
						<a-col :span="24" :lg="12">
							<a-button type="primary" block @click="openStateNotification('primary')">
								Primary
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-secondary" block @click="openStateNotification('secondary')">
								Secondary
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-success" block @click="openStateNotification('success')">
								Success
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-warning" block @click="openStateNotification('warning')">
								Warning
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button type="danger" block @click="openStateNotification('danger')">
								Danger
							</a-button>
						</a-col>
						
						<a-col :span="24" :lg="12">
							<a-button class="bg-dark" block @click="openStateNotification('dark')">
								Dark
							</a-button>
						</a-col>

					</a-row>

				</a-card>
			</a-col>
			<!-- / Solid State Notifications -->
			
		</a-row>

	</div>
</template>

<script>

	export default ({
		data() {
			return {
			}
		},
		methods: {

			// Method for opening "Basic" notification.
			openBasicNotification() {
				this.$notification.open({
					message: 'Notification Title',
					description:
					'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
					onClick: () => {
						console.log('Notification Clicked!');
					},
				});
			},

			// Method for opening "Customized Icon" notification.
			openCustomizedIconNotification() {
				this.$notification.open({
					message: 'Notification Title',
					description:
					'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
					icon: <a-icon type="smile" style="color: #108ee9" />,
				});
			},

			// Method for opening "Customized style" notification.
			openCustomizedStyleNotification() {
				this.$notification.open({
					message: 'Notification Title',
					description:
					'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
					style: {
						width: '600px',
						marginLeft: `${335 - 600}px`,
					},
				});
			},

			// Method for opening "Custom duration" notification.
			openCustomDurationNotification() {
				this.$notification.open({
					message: 'Notification Title',
					description:
					'I will never close automatically. I will be close automatically. I will never close automatically.',
					duration: 0,
				});
			},

			// Method for opening "Various Positions" notifications.
			openCustomPositionedNotification(placement) {
				this.$notification.open({
					message: `Notification ${placement}`,
					description:
					'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
					placement,
				});
			},

			// Method for opening "Custom close button" notification.
			openCustomCloseButtonNotification() {

				const close = () => {
					console.log(
						'Notification was closed. Either the close button was clicked or duration time elapsed.',
					);
				};

				const key = `open${Date.now()}`;

				this.$notification.open({
					message: 'Notification Title',
					description:
					'A function will be be called after the notification is closed (automatically after the "duration" time of manually).',
					btn: h => {
					return h(
						'a-button',
						{
						props: {
							type: 'primary',
							size: 'small',
						},
						on: {
							click: () => this.$notification.close(key),
						},
						},
						'Confirm',
					);
					},
					key,
					onClose: close,
				});

			},

			// Method for opening "Update Message Content" notification.
			openUpdateMessageContentNotification() {
				
				const key = 'updatable';

				this.$notification.open({
					key,
					message: 'Notification Title',
					description: 'description.',
				});

				setTimeout(() => {
					this.$notification.open({
					key,
					message: 'New Title',
					description: 'New description.',
					});
				}, 1000);

			},

			// Method for opening "Various States" notification.
			openNotificationWithIcon(type) {
				this.$notification[type]({
					message: 'Notification Title',
					description:
					'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
				});
			},

			// Method for opening "Solid State" notification.
			openStateNotification(type) {
				this.$notification.open({
					class: 'ant-notification-' + type,
					message: 'Notification Title',
					description: 'This is the content of the notification. This is the content of the notification. This is the content of the notification.',
					duration: 0,
				});
			},

		},
	})

</script>

<style lang="scss" scoped>
	.ant-btn.bg-info {
		background-color: #4bc2fc;
		color: #ffffff;

		&:hover {
			color: #ffffff;
			background-color: #3da6d9;
		}
	}
</style>