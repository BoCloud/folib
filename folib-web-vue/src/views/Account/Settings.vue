<!-- 
	This is the Settings page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div id="settings">
		
		<a-tabs class="tabs-sliding" default-active-key="1">

			<!-- Default Tab ( Messages ) -->
			<a-tab-pane key="1" tab="Messages">
				
				<a-row type="flex" :gutter="[24,24]">

					<a-col :span="24" :lg="6">

						<!-- Page Anchors -->
						<a-affix :offset-top=" navbarFixed ? 100 : 10 ">
							<a-card :bordered="false" class="header-solid mb-24">
								<a-anchor :targetOffset=" navbarFixed ? 100 : 10 " :affix="false">
									<a-anchor-link href="#profile">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="appstore" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Profile</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#basic-info">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="snippets" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Basic Info</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#change-password">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="unlock" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Change Password</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#2fa">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="safety-certificate" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">2FA</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#accounts">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="dashboard" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Accounts</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#notifications">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="notification" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Notifications</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#sessions">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="wallet" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Sessions</span>
											</h4>
										</div>
									</a-anchor-link>
									<a-anchor-link href="#delete-account">
										<div slot="title" class="ant-list-item-meta">
											<a-icon type="delete" theme="filled" class="text-gray-6 text-lg" />
											<h4 class="ant-list-item-meta-title">
												<span class="font-regular">Delete Account</span>
											</h4>
										</div>
									</a-anchor-link>
								</a-anchor>
							</a-card>
						</a-affix>
						<!-- / Page Anchors -->

					</a-col>
					<a-col :span="24" :lg="18">

						<!-- User Profile card -->
						<a-card :bordered="false" id="profile" class="card-profile-head" :bodyStyle="{padding: 0,}">
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
										<small class="mr-5">Switch to <span class="font-regular m-0" v-if="visibility">in</span>visible</small>
    									<a-switch default-checked v-model="visibility" />
									</a-col>
								</a-row>
							</template>
						</a-card>
						<!-- User Profile card -->

						<!-- Basic Info card -->
						<a-card :bordered="false" id="basic-info" class="header-solid mb-24">
							<template #title>
								<h5 class="mb-0 font-semibold">Basic Info</h5>
							</template>
							<a-form
								:hideRequiredMark="true"
							>
								<a-row :gutter="[24]">
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="First Name" :colon="false">
											<a-input placeholder="Alec" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Last Name" :colon="false">
											<a-input placeholder="Thompson" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="8">
										<a-form-item class="mb-10" label="I'm" :colon="false">
											<a-select
												show-search
												placeholder="Select a person"
												option-filter-prop="children"
												:filter-option="filterOption"
											>
												<a-select-option value="jack">
												Jack
												</a-select-option>
												<a-select-option value="lucy">
												Lucy
												</a-select-option>
												<a-select-option value="tom">
												Tom
												</a-select-option>
											</a-select>
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="16">
										<a-row :gutter="[24]">
											<a-col :span="24" :lg="10">
												<a-form-item class="mb-10" label="Birth Date" :colon="false">
													<a-select
														show-search
														option-filter-prop="children"
														:defaultValue="'January'"
														:filter-option="filterOption"
													>
														<a-select-option value="January">January</a-select-option>
														<a-select-option value="February">February</a-select-option>
														<a-select-option value="March">March</a-select-option>
														<a-select-option value="April">April</a-select-option>
														<a-select-option value="May">May</a-select-option>
														<a-select-option value="June">June</a-select-option>
														<a-select-option value="July">July</a-select-option>
														<a-select-option value="August">August</a-select-option>
														<a-select-option value="September">September</a-select-option>
														<a-select-option value="October">October</a-select-option>
														<a-select-option value="November">November</a-select-option>
														<a-select-option value="December">December</a-select-option>
													</a-select>
												</a-form-item>
											</a-col>
											<a-col :span="24" :lg="8">
												<a-form-item class="mb-10" label=" " :colon="false">
													<a-select
														show-search
														placeholder="Select a day"
														option-filter-prop="children"
														:filter-option="filterOption"
													>
														<a-select-option v-for="item in days" :key="item" :value="item">{{ item }}</a-select-option>
													</a-select>
												</a-form-item>
											</a-col>
											<a-col :span="24" :lg="6">
												<a-form-item class="mb-10" label=" " :colon="false">
													<a-select
														show-search
														placeholder="Select a year"
														option-filter-prop="children"
														:filter-option="filterOption"
													>
														<a-select-option v-for="item in years" :key="item" :value="item">{{ item }}</a-select-option>
													</a-select>
												</a-form-item>
											</a-col>
										</a-row>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Email" :colon="false">
											<a-input placeholder="example@email.com" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Confirmation Email" :colon="false">
											<a-input placeholder="example@email.com" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Your location" :colon="false">
											<a-input placeholder="Sydney, A" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Phone Number" :colon="false">
											<a-input placeholder="+40 735 631 620" />
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="mb-10" label="Language" :colon="false">
											<a-select
												show-search
												placeholder="Select a language"
												option-filter-prop="children"
												:filter-option="filterOption"
											>
												<a-select-option value="English">English</a-select-option>
												<a-select-option value="French">French</a-select-option>
												<a-select-option value="Spanish">Spanish</a-select-option>
											</a-select>
										</a-form-item>
									</a-col>
									<a-col :span="24" :lg="12">
										<a-form-item class="tags-field mb-10" label="Skills" :colon="false">
											<a-select mode="tags" :defaultValue="tags" style="width: 100%" placeholder="Skills">
												<a-select-option v-for="tag in tags" :key="tag" :value="tag">
													{{ tag }}
												</a-select-option>
											</a-select>
										</a-form-item>
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Basic Info card -->

						<!-- Change Password card -->
						<a-card :bordered="false" id="change-password" class="header-solid mb-24">
							<template #title>
								<h5 class="mb-0 font-semibold">Change Password</h5>
							</template>
							<a-form
								:hideRequiredMark="true"
							>
								<a-row :gutter="[24]">
									<a-col :span="24">
										<a-form-item class="mb-10" label="Current password" :colon="false">
											<a-input-password placeholder="Current password" />
										</a-form-item>
									</a-col>
									<a-col :span="24">
										<a-form-item class="mb-10" label="New password" :colon="false">
											<a-input-password placeholder="New password" />
										</a-form-item>
									</a-col>
									<a-col :span="24">
										<a-form-item class="mb-10" label="Confirm new password" :colon="false">
											<a-input-password placeholder="Confirm new password" />
										</a-form-item>
									</a-col>
								</a-row>
								<h5 class="font-semibold mt-20">Password requirements</h5>
								<p>Please follow this guide for a strong password:</p>
								<ul class="pl-15 text-muted">
									<li>One special characters</li>
									<li>Min 6 characters</li>
									<li>One number (2 are recommended)</li>
									<li>Change it often</li>
								</ul>
								<a-row :gutter="[24]">
									<a-col :span="24" :lg="12">
									</a-col>
									<a-col :span="24" :lg="12" class="text-right">
										<a-button type="primary" class="px-25">UPDATE PASSWORD</a-button>
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Change Password card -->


						<!-- Two-factor authentication card -->
						<a-card :bordered="false" id="2fa" class="header-solid mb-24">
							<template #title>
								<h5 class="mb-0 font-semibold">Two-factor authentication</h5>
							</template>
							<a-tag slot="extra" color="success" class="ant-tag-success px-10 py-5 font-bold">ENABLED</a-tag>
							<a-form
								:hideRequiredMark="true"
							>
								<a-row type="flex" align="middle">
									<a-col>
										<h6 class="font-regular mb-0">Security keys</h6>
									</a-col>
									<a-col class="ml-auto">
										<span class="mr-15">No Security Keys</span>
    									<a-button size="small" class="font-bold px-25">ADD</a-button>
									</a-col>
								</a-row>
								<hr class="gradient-line">
								<a-row type="flex" align="middle">
									<a-col>
										<h6 class="font-regular mb-0">SMS number</h6>
									</a-col>
									<a-col class="ml-auto">
										<span class="mr-15">+4012374423</span>
    									<a-button size="small" class="font-bold px-25">EDIT</a-button>
									</a-col>
								</a-row>
								<hr class="gradient-line">
								<a-row type="flex" align="middle">
									<a-col>
										<h6 class="font-regular mb-0">Authenticator app</h6>
									</a-col>
									<a-col class="ml-auto">
										<span class="mr-15">Not Configured</span>
    									<a-button size="small" class="font-bold px-25">SET UP</a-button>
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Two-factor authentication card -->

						<!-- Accounts card -->
						<a-card :bordered="false" id="accounts" class="header-solid mb-24">
							<template #title>
								<h5 class="font-semibold">Accounts</h5>
								<p class="font-regular">Here you can setup and manage your integration settings.</p>
							</template>
							<a-form
								:hideRequiredMark="true"
							>
								<a-row type="flex" align="middle">
									<a-col>
      									<a-avatar :size="48" src="images/small-logos/logo-slack.svg" />
									</a-col>
									<a-col class="pl-15">
										<h5 class="mb-0">Slack</h5>
										<a class="text-dark">Show less <a-icon type="up" class="ml-5" /></a>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="mr-15">{{ slack ? 'Enabled' : 'Disabled' }}</span>
    									<a-switch default-checked v-model="slack" />
									</a-col>
								</a-row>
								<a-row type="flex" class="mt-15" style="flex-wrap: nowrap;">
									<a-col flex="1 1 100px"></a-col>
									<a-col flex="1 1 auto">
										<p>
											You haven't added your Slack yet or you aren't authorized. Please add our Slack Bot to your account by clicking on here. When you've added the bot, send your verification code that you have received.
										</p>
										
										<a-card :bordered="false" class="bg-gray-3 shadow-0 mb-24" :bodyStyle="{padding: '8px'}">
											<a-row type="flex" align="middle">
											  	<a-col><p class="font-semibold mb-0 ml-10">Verification Code</p></a-col>
											  	<a-col class="ml-auto">
													<a-input value="1172913" size="small" class="font-regular text-sm text-dark" style="width: 100px;" />
												</a-col>
											</a-row>
										</a-card>
										
										<a-card :bordered="false" class="bg-gray-3 shadow-0" :bodyStyle="{padding: '8px'}">
											<a-row type="flex" align="middle">
											  	<a-col><p class="font-semibold mb-0 ml-10">Verification Code</p></a-col>
											  	<a-col class="ml-auto">
													<strong class="font-semibold">hello@creative-tim.com</strong>
													<a-button size="small" type="danger" class="ml-10 px-25 font-bold">DELETE</a-button>
												</a-col>
											</a-row>
										</a-card>
									</a-col>
								</a-row>
								<hr class="gradient-line">
								<a-row type="flex" align="middle">
									<a-col>
      									<a-avatar :size="48" src="images/small-logos/logo-spotify.svg" />
									</a-col>
									<a-col class="pl-15">
										<h5 class="mb-0">Spotify</h5>
										<a class="text-dark">Music</a>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="mr-15">{{ spotify ? 'Enabled' : 'Disabled' }}</span>
    									<a-switch default-checked v-model="spotify" />
									</a-col>
								</a-row>
								<hr class="gradient-line">
								<a-row type="flex" align="middle">
									<a-col>
      									<a-avatar :size="48" src="images/small-logos/logo-atlassian.svg" />
									</a-col>
									<a-col class="pl-15">
										<h5 class="mb-0">Atlassian</h5>
										<a class="text-dark">Payment vendor</a>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="mr-15">{{ atlassian ? 'Enabled' : 'Disabled' }}</span>
    									<a-switch default-checked v-model="atlassian" />
									</a-col>
								</a-row>
								<hr class="gradient-line">
								<a-row type="flex" align="middle">
									<a-col>
      									<a-avatar :size="48" src="images/small-logos/logo-asana.svg" />
									</a-col>
									<a-col class="pl-15">
										<h5 class="mb-0">Asana</h5>
										<a class="text-dark">Organize your team</a>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
    									<a-switch default-checked v-model="asana" />
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Accounts card -->

						<!-- Notifications card -->
						<a-card :bordered="false" id="notifications" class="header-solid mb-24">
							<template #title>
								<h5 class="font-semibold">Notifications</h5>
								<p class="font-regular">Choose how you receive notifications. These notification settings apply to the things you’re watching.</p>
							</template>
							<a-form
								:hideRequiredMark="true"
							>
								<table class="table-settings-notifications">
									<thead>
										<tr>
											<th colspan="4">Activity</th>
											<th>Email</th>
											<th>Push</th>
											<th>SMS</th>
										</tr>
									</thead>
									<tbody>
										<tr>
											<td colspan="4">
												<p>Mentions</p>
												<small>Notify when another user mentions you in a comment</small>
											</td>
											<td><a-switch default-checked /></td>
											<td><a-switch /></td>
											<td><a-switch /></td>
										</tr>
										<tr>
											<td colspan="4">
												<p>Comments</p>
												<small>Notify when another user comments your item.</small>
											</td>
											<td><a-switch default-checked /></td>
											<td><a-switch default-checked /></td>
											<td><a-switch /></td>
										</tr>
										<tr>
											<td colspan="4">
												<p>Follows</p>
												<small>Notify when another user follows you.</small>
											</td>
											<td><a-switch /></td>
											<td><a-switch default-checked /></td>
											<td><a-switch /></td>
										</tr>
										<tr>
											<td colspan="4">
												<p>Log in from a new device</p>
												<small></small>
											</td>
											<td><a-switch default-checked /></td>
											<td><a-switch default-checked /></td>
											<td><a-switch default-checked /></td>
										</tr>
									</tbody>
								</table>
							</a-form>
						</a-card>
						<!-- / Notifications card -->

						<!-- Sessions card -->
						<a-card :bordered="false" id="sessions" class="header-solid mb-24">
							<template #title>
								<h5 class="font-semibold">Sessions</h5>
								<p class="font-regular">This is a list of devices that have logged into your account. Remove those that you do not recognize.</p>
							</template>
							<a-form
								class="list-settings-sessions"
								:hideRequiredMark="true"
							>
								<a-row type="flex" align="middle">
									<a-col style="min-width: 40px;" class="text-center">
										<a-icon type="desktop" class="text-gray-6" style="font-size: 18px;" />
									</a-col>
									<a-col class="pl-15">
										<p class="mb-0">Confirm</p>
										<small class="text-dark">Your current session</small>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<a-tag color="success" class="ant-tag-success font-bold">ACTIVE</a-tag>
										<span class="ml-5">EU</span>
										<a-button type="link" class="btn-more ml-5">
											See More <a-icon type="arrow-right" />
										</a-button>
									</a-col>
								</a-row>
								<hr class="gradient-line my-10">
								<a-row type="flex" align="middle">
									<a-col style="min-width: 40px;" class="text-center">
										<a-icon type="desktop" class="text-gray-6" style="font-size: 18px;" />
									</a-col>
									<a-col class="pl-15">
										<p class="mb-0">Chrome on macOS</p>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="ml-5">US</span>
										<a-button type="link" class="btn-more ml-5">
											See More <a-icon type="arrow-right" />
										</a-button>
									</a-col>
								</a-row>
								<hr class="gradient-line my-10">
								<a-row type="flex" align="middle">
									<a-col style="min-width: 40px;" class="text-center">
										<a-icon type="mobile" class="text-gray-6" style="font-size: 18px;" />
									</a-col>
									<a-col class="pl-15">
										<p class="mb-0">Safari on iPhone</p>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<span class="ml-5">US</span>
										<a-button type="link" class="btn-more ml-5">
											See More <a-icon type="arrow-right" />
										</a-button>
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Sessions card -->

						<!-- Delete Account card -->
						<a-card :bordered="false" id="delete-account" class="header-solid mb-24">
							<template #title>
								<h5 class="font-semibold">Delete Account</h5>
								<p class="font-regular">Once you delete your account, there is no going back. Please be certain.</p>
							</template>
							<a-form
								id="components-form-demo-normal-login"
								class="login-form list-settings-sessions"
								:hideRequiredMark="true"
							>
								<a-row type="flex" align="middle">
									<a-col style="min-width: 40px;" class="text-center">
										<a-switch></a-switch>
									</a-col>
									<a-col class="pl-15">
										<p class="mb-0 font-semibold">Confirm</p>
										<small class="text-dark">I want to delete my account.</small>
									</a-col>
									<a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
										<a-button>
											DEACTIVATE
										</a-button>
										<a-button type="danger" class="ml-10">
											DELETE ACCOUNT
										</a-button>
									</a-col>
								</a-row>
							</a-form>
						</a-card>
						<!-- / Delete Account card -->
						
					</a-col>

				</a-row>

			</a-tab-pane>
			<!-- / Default Tab ( Messages ) -->

			<!-- Social Tab -->
			<a-tab-pane key="2" tab="Social">

				<a-card class="header-solid" :bodyStyle="{padding: '50px', height: '300px', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
					<a href="#" class="text-center text-muted font-bold">
						<h6 class="font-semibold text-muted">Empty Tab</h6>
					</a>
				</a-card>

			</a-tab-pane>
			<!-- / Social Tab -->

			<!-- Notifications Tab -->
			<a-tab-pane key="3" tab="Notifications">

				<a-card class="header-solid" :bodyStyle="{padding: '50px', height: '300px', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
					<a href="#" class="text-center text-muted font-bold">
						<h6 class="font-semibold text-muted">Empty Tab</h6>
					</a>
				</a-card>

			</a-tab-pane>
			<!-- / Notifications Tab -->

			<!-- Backup Tab -->
			<a-tab-pane key="4" tab="Backup">

				<a-card class="header-solid" :bodyStyle="{padding: '50px', height: '300px', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
					<a href="#" class="text-center text-muted font-bold">
						<h6 class="font-semibold text-muted">Empty Tab</h6>
					</a>
				</a-card>

			</a-tab-pane>
			<!-- / Backup Tab -->

		</a-tabs>

	</div>

</template>

<script>

	export default {
		props: [ 'navbarFixed' ],
		data() {
			return {
				visibility: true,
				slack: true,
				spotify: true,
				atlassian: true,
				asana: false,
				tags: ['Vuejs', 'Angular', 'React'],
			};
		},
		computed: {
			days(){
				let days = [] ;

				for( let i = 1 ; i <= 31 ; i++ ){
					days.push( i ) ;
				}

				return days ;
			},
			years(){
				let years = [] ;

				for( let i = 1930 ; i <= (new Date()).getFullYear() ; i++ ){
					years.push( i ) ;
				}

				return years ;
			}
		},
		methods: {
			// Languages select field search method.
			filterOption(input, option) {
				return (
					option.componentOptions.children[0].text.toLowerCase().indexOf(input.toLowerCase()) >= 0
				);
			},
		},
	};
</script>

<style lang="scss" scoped>
	#settings::v-deep {
		.ant-list {
			width: 100%;
		}
		.ant-list-item-meta-avatar {
			margin-right: 8px; 
		}
		.ant-list-item-meta {
			align-items: center;
		}
		.ant-list-item-meta-title {
			margin: 0;
		}

		.ant-anchor-ink::before {
			display: none;
		}
		.ant-anchor-link {
			padding: 0;
			margin-top: 8px;
		}
		.ant-anchor-link a {
			width: 100%;
			border-radius: 8px;
			color: #67748e !important;
			padding: 10px 16px;
			background-color: transparent;
			transition: background-color 0.3s ease-in;
		}
		.ant-anchor-link a:hover {
			background-color: #eeeeee;
		}
		.ant-anchor-link a svg g {
			fill: #344767;
		}
		.ant-anchor-link a svg {
			margin-right: 8px;
		}

		.card-profile-head {
			margin: 0 0 24px;
		}
		.tags-field .ant-form-item-control {
			line-height: 33px;
		}
		.form-tag.ant-tag {
			border-radius: 20px;
			padding: 4px 10px;
			font-size: 12px;
			font-weight: 500;
			margin-right: 3.75px;
			margin-bottom: 3.75px;
			background-color: #3a416f;
			border: 1px solid #3a416f;
			color: #fff;
		}
		.form-tag.ant-tag .anticon-close {
			color: #fff;
			height: 16px;
			border-left: 1px solid hsla(0,0%,100%,.3);
			padding-left: 5px;
			padding-top: 2px;
			opacity: .75;
		}
		.form-tag.ant-tag .anticon-close:hover {
			color: #fff;
			opacity: 1;
		}
		.tags-field .ant-input {
			margin-bottom: 5px;
			margin-top: 4px;
		}

		.tags-field .ant-select {
			.ant-select-selection__choice__remove i {
				color: #fff;
				height: 16px;
				border-left: 1px solid hsla(0,0%,100%,.3);
				padding-left: 5px;
				padding-top: 2px;
				opacity: .75;

				&:hover {
					color: #fff;
					opacity: 1;
				}
			}
			.ant-select-selection__rendered > ul > li:not(.ant-select-search) {
				border-radius: 20px;
				padding: 2px 27px 2px 10px;
				font-size: 12px;
				font-weight: 500;
				margin-right: 3.75px;
				margin-bottom: 3.75px;
				background-color: #3a416f;
				border: 1px solid #3a416f;
				color: #fff;
				line-height: 2;
				height: 30px;
			}
			.ant-select-selection--multiple {
				padding: 8px 10px;
			}
		}
	}

</style>