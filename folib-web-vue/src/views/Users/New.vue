<!-- 
	This is the new user page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div class="mx-auto mt-50" style="max-width: 800px;">

		<div class="mb-50" style="max-width: 800px;">

			<a-steps progress-dot v-model="step">
				<a-step title="User Info" />
				<a-step title="Address" />
				<a-step title="Socials" />
				<a-step title="Profile" />
			</a-steps>

		</div>

		<div class="mb-24">
			<!-- Step 1 -->
			<a-card v-if="step == 0" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
				<template #title>
					<h5 class="mb-0">About me</h5>
					<p class="font-regular">Mandatory informations</p>
				</template>
				<a-form
					:form="form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="12">
							<a-form-item class="mb-10" label="First Name" :colon="false">
								<a-input placeholder="eg. Michael" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="Last Name" :colon="false">
								<a-input placeholder="eg. Prior" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="Company" :colon="false">
								<a-input placeholder="eg. Creative Tim" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="Email Address" :colon="false">
								<a-input placeholder="eg. soft@dashboard.com" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="Password" :colon="false">
								<a-input placeholder="******" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="Repeat Password" :colon="false">
								<a-input placeholder="******" />
							</a-form-item>
						</a-col>
					</a-row>
					<a-row :gutter="[24]">
						<a-col :span="12">
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button type="primary" @click="moveStep(1)" class="px-25">NEXT</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-card>
			<!-- Step 2 -->
			<a-card v-else-if="step == 1" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
				<template #title>
					<h5 class="mb-0">Address</h5>
				</template>
				<a-form
					:form="form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="24">
							<a-form-item class="mb-10" label="Address 1" :colon="false">
								<a-input placeholder="eg. Street 111" />
							</a-form-item>
						</a-col>
						<a-col :span="24">
							<a-form-item class="mb-10" label="Address 2" :colon="false">
								<a-input placeholder="eg. Street 221" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-form-item class="mb-10" label="City" :colon="false">
								<a-input placeholder="eg. Tokyo" />
							</a-form-item>
						</a-col>
						<a-col :span="6">
							<a-form-item class="mb-10" label="State" :colon="false">
								<a-select default-value="lucy">
									<a-select-option value="jack">
										Jack
									</a-select-option>
									<a-select-option value="lucy">
										Lucy
									</a-select-option>
									<a-select-option value="disabled" disabled>
										Disabled
									</a-select-option>
									<a-select-option value="Yiminghe">
										yiminghe
									</a-select-option>
								</a-select>
							</a-form-item>
						</a-col>
						<a-col :span="6">
							<a-form-item class="mb-10" label="Zip" :colon="false">
								<a-input placeholder="7 letters" />
							</a-form-item>
						</a-col>
					</a-row>
					<a-row :gutter="[24]">
						<a-col :span="12">
							<a-button @click="moveStep(-1)" class="px-25">PREV</a-button>
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button type="primary" @click="moveStep(1)" class="px-25">NEXT</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-card>
			<!-- Step 3 -->
			<a-card v-else-if="step == 2" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
				<template #title>
					<h5 class="mb-0">Socials</h5>
				</template>
				<a-form
					:form="form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="24">
							<a-form-item class="mb-10" label="Twitter Handle" :colon="false">
								<a-input placeholder="@Soft" />
							</a-form-item>
						</a-col>
						<a-col :span="24">
							<a-form-item class="mb-10" label="Facebook Account" :colon="false">
								<a-input placeholder="http://..." />
							</a-form-item>
						</a-col>
						<a-col :span="24">
							<a-form-item class="mb-10" label="Instagram Account" :colon="false">
								<a-input placeholder="http://..." />
							</a-form-item>
						</a-col>
					</a-row>
					<a-row :gutter="[24]">
						<a-col :span="12">
							<a-button @click="moveStep(-1)" class="px-25">PREV</a-button>
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button type="primary" @click="moveStep(1)" class="px-25">NEXT</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-card>
			<!-- Step 4 -->
			<a-card v-else-if="step == 3" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
				<template #title>
					<h5 class="mb-0">Profile</h5>
				</template>
				<a-form
					:form="form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="24">
							<a-form-item class="mb-10" label="Public Email" :colon="false">
								<a-input placeholder="Use an address you don't use frequently." />
							</a-form-item>
						</a-col>
						<a-col :span="24">
							<a-form-item class="mb-10" label="Bio" :colon="false">
								<a-textarea :rows="5" placeholder="Say a few words about who you are or what you're working on." />
							</a-form-item>
						</a-col>
					</a-row>
					<a-row :gutter="[24]">
						<a-col :span="12">
							<a-button @click="moveStep(-1)" class="px-25">PREV</a-button>
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button type="primary" class="px-25">SEND</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-card>
		</div>

	</div>

</template>

<script>

	export default {
		components: {
		},
		data() {
			return {
				step: 0,
				// Binded model property for "Sign In Form" switch button for "Remember Me" .
				rememberMe: true,
			}
		},
		beforeCreate() {
			// Creates the form and adds to it component's "form" property.
			this.form = this.$form.createForm(this, { name: 'user' });
		},
		methods: {
			// Handles input validation after submission.
			handleSubmit(e) {
				e.preventDefault();
				this.form.validateFields((err, values) => {
					if ( !err ) {
						console.log('Received values of form: ', values) ;
					}
				});
			},
			moveStep( distance ) {
				this.step += distance ;
			}
		},
	}

</script>

<style lang="scss">
</style>