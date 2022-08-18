<!-- 
	This is the Wizard page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div class="mx-auto m-50" style="max-width: 800px;">

		<!-- Header -->
		<h3 class="mt-25 mb-5 text-center">Build Your Profile</h3>
		<h5 class="text-center font-regular">This information will let us know more about you.</h5>

		<div class="my-50" style="max-width: 800px;">

			<!-- Steps -->
			<a-steps progress-dot v-model="step">
				<a-step title="About" />
				<a-step title="Account" />
				<a-step title="Address" />
			</a-steps>
			<!-- / Steps -->

		</div>
		<!-- / Header -->

		<!-- Wizard form cards -->
		<div class="mb-50">

			<!-- Step 1 : About -->
			<a-card v-if="step == 0" :bordered="false" class="header-solid">

				<h5 class="font-regular text-center">Let's start with the basic information</h5>
				<p class="text-center">Let us know your name and email address. Use an address you don't mind other users contacting you at</p>

				<a-form
					:form="form"
					class="mt-30"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="24" :md="8" class="text-center">
							<div class="avatar-upload">
	  							<a-avatar shape="square" :size="110" src="images/team-2.jpg" />
								<a-button icon="edit"></a-button>
							</div>
						</a-col>
						<a-col :span="24" :md="16">
							<a-form-item class="mb-10" label="First Name" :colon="false">
								<a-input placeholder="eg. Prior" />
							</a-form-item>
							<a-form-item class="mb-10" label="Last Name" :colon="false">
								<a-input placeholder="eg. Prior" />
							</a-form-item>
							<a-form-item class="mb-10" label="Email Address" :colon="false">
								<a-input placeholder="eg. soft@dashboard.com" />
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
			
			<!-- Step 2 : Account -->
			<a-card v-else-if="step == 1" :bordered="false" class="header-solid">

				<h5 class="font-regular text-center">What are you doing? (checkboxes)</h5>
				<p class="text-center">Give us more details about you. What do you enjoy doing in your spare time?</p>

				<a-form
					:form="form"
					class="mt-30"
					:hideRequiredMark="true"
				>
					<a-row type="flex" :gutter="[24]">
						<a-col :span="24" :md="20" :lg="18" class="mx-auto">
							<a-row class="checkbox-group" type="flex" :gutter="[50]">
								<a-col :span="8">
									<div class="checkbox-label"
										:class="[checkedList.includes('Design') ? 'active' : '']"
										@click="toggleCheckbox('Design')"
									>
										<svg width="20px" height="20px" viewBox="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
											<title>settings</title>
											<g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
												<g transform="translate(-2020.000000, -442.000000)" class="fill-dark" fill="#FFFFFF" fill-rule="nonzero">
												<g transform="translate(1716.000000, 291.000000)">
													<g transform="translate(304.000000, 151.000000)">
													<polygon class="color-background" opacity="0.596981957" points="18.0883333 15.7316667 11.1783333 8.82166667 13.3333333 6.66666667 6.66666667 0 0 6.66666667 6.66666667 13.3333333 8.82166667 11.1783333 15.315 17.6716667"></polygon>
													<path class="color-background" d="M31.5666667,23.2333333 C31.0516667,23.2933333 30.53,23.3333333 30,23.3333333 C29.4916667,23.3333333 28.9866667,23.3033333 28.48,23.245 L22.4116667,30.7433333 L29.9416667,38.2733333 C32.2433333,40.575 35.9733333,40.575 38.275,38.2733333 L38.275,38.2733333 C40.5766667,35.9716667 40.5766667,32.2416667 38.275,29.94 L31.5666667,23.2333333 Z" opacity="0.596981957"></path>
													<path class="color-background" d="M33.785,11.285 L28.715,6.215 L34.0616667,0.868333333 C32.82,0.315 31.4483333,0 30,0 C24.4766667,0 20,4.47666667 20,10 C20,10.99 20.1483333,11.9433333 20.4166667,12.8466667 L2.435,27.3966667 C0.95,28.7083333 0.0633333333,30.595 0.00333333333,32.5733333 C-0.0583333333,34.5533333 0.71,36.4916667 2.11,37.89 C3.47,39.2516667 5.27833333,40 7.20166667,40 C9.26666667,40 11.2366667,39.1133333 12.6033333,37.565 L27.1533333,19.5833333 C28.0566667,19.8516667 29.01,20 30,20 C35.5233333,20 40,15.5233333 40,10 C40,8.55166667 39.685,7.18 39.1316667,5.93666667 L33.785,11.285 Z"></path>
													</g>
												</g>
												</g>
											</g>
										</svg>
									</div>
									<h6>Design</h6>
								</a-col>
								<a-col :span="8">
									<div class="checkbox-label"
										:class="[checkedList.includes('Code') ? 'active' : '']"
										@click="toggleCheckbox('Code')"
									>
										<svg width="20px" height="20px" viewBox="0 0 42 42" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
											<title>box-3d-50</title>
											<g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
												<g transform="translate(-2319.000000, -291.000000)" class="fill-dark" fill="#FFFFFF" fill-rule="nonzero">
												<g transform="translate(1716.000000, 291.000000)">
													<g transform="translate(603.000000, 0.000000)">
													<path class="color-background" d="M22.7597136,19.3090182 L38.8987031,11.2395234 C39.3926816,10.9925342 39.592906,10.3918611 39.3459167,9.89788265 C39.249157,9.70436312 39.0922432,9.5474453 38.8987261,9.45068056 L20.2741875,0.1378125 L20.2741875,0.1378125 C19.905375,-0.04725 19.469625,-0.04725 19.0995,0.1378125 L3.1011696,8.13815822 C2.60720568,8.38517662 2.40701679,8.98586148 2.6540352,9.4798254 C2.75080129,9.67332903 2.90771305,9.83023153 3.10122239,9.9269862 L21.8652864,19.3090182 C22.1468139,19.4497819 22.4781861,19.4497819 22.7597136,19.3090182 Z"></path>
													<path class="color-background" d="M23.625,22.429159 L23.625,39.8805372 C23.625,40.4328219 24.0727153,40.8805372 24.625,40.8805372 C24.7802551,40.8805372 24.9333778,40.8443874 25.0722402,40.7749511 L41.2741875,32.673375 L41.2741875,32.673375 C41.719125,32.4515625 42,31.9974375 42,31.5 L42,14.241659 C42,13.6893742 41.5522847,13.241659 41,13.241659 C40.8447549,13.241659 40.6916418,13.2778041 40.5527864,13.3472318 L24.1777864,21.5347318 C23.8390024,21.7041238 23.625,22.0503869 23.625,22.429159 Z" opacity="0.7"></path>
													<path class="color-background" d="M20.4472136,21.5347318 L1.4472136,12.0347318 C0.953235098,11.7877425 0.352562058,11.9879669 0.105572809,12.4819454 C0.0361450918,12.6208008 6.47121774e-16,12.7739139 0,12.929159 L0,30.1875 L0,30.1875 C0,30.6849375 0.280875,31.1390625 0.7258125,31.3621875 L19.5528096,40.7750766 C20.0467945,41.0220531 20.6474623,40.8218132 20.8944388,40.3278283 C20.963859,40.1889789 21,40.0358742 21,39.8806379 L21,22.429159 C21,22.0503869 20.7859976,21.7041238 20.4472136,21.5347318 Z" opacity="0.7"></path>
													</g>
												</g>
												</g>
											</g>
										</svg>
									</div>
									<h6>Code</h6>
								</a-col>
								<a-col :span="8">
									<div class="checkbox-label"
										:class="[checkedList.includes('Develop') ? 'active' : '']"
										@click="toggleCheckbox('Develop')"
									>
										<svg width="20px" height="20px" viewBox="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
											<title>spaceship</title>
											<g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
												<g transform="translate(-1720.000000, -592.000000)" class="fill-dark" fill="#FFFFFF" fill-rule="nonzero">
												<g transform="translate(1716.000000, 291.000000)">
													<g transform="translate(4.000000, 301.000000)">
													<path class="color-background" d="M39.3,0.706666667 C38.9660984,0.370464027 38.5048767,0.192278529 38.0316667,0.216666667 C14.6516667,1.43666667 6.015,22.2633333 5.93166667,22.4733333 C5.68236407,23.0926189 5.82664679,23.8009159 6.29833333,24.2733333 L15.7266667,33.7016667 C16.2013871,34.1756798 16.9140329,34.3188658 17.535,34.065 C17.7433333,33.98 38.4583333,25.2466667 39.7816667,1.97666667 C39.8087196,1.50414529 39.6335979,1.04240574 39.3,0.706666667 Z M25.69,19.0233333 C24.7367525,19.9768687 23.3029475,20.2622391 22.0572426,19.7463614 C20.8115377,19.2304837 19.9992882,18.0149658 19.9992882,16.6666667 C19.9992882,15.3183676 20.8115377,14.1028496 22.0572426,13.5869719 C23.3029475,13.0710943 24.7367525,13.3564646 25.69,14.31 C26.9912731,15.6116662 26.9912731,17.7216672 25.69,19.0233333 L25.69,19.0233333 Z"></path>
													<path class="color-background" d="M1.855,31.4066667 C3.05106558,30.2024182 4.79973884,29.7296005 6.43969145,30.1670277 C8.07964407,30.6044549 9.36054508,31.8853559 9.7979723,33.5253085 C10.2353995,35.1652612 9.76258177,36.9139344 8.55833333,38.11 C6.70666667,39.9616667 0,40 0,40 C0,40 0,33.2566667 1.855,31.4066667 Z"></path>
													<path class="color-background" d="M17.2616667,3.90166667 C12.4943643,3.07192755 7.62174065,4.61673894 4.20333333,8.04166667 C3.31200265,8.94126033 2.53706177,9.94913142 1.89666667,11.0416667 C1.5109569,11.6966059 1.61721591,12.5295394 2.155,13.0666667 L5.47,16.3833333 C8.55036617,11.4946947 12.5559074,7.25476565 17.2616667,3.90166667 L17.2616667,3.90166667 Z" opacity="0.598539807"></path>
													<path class="color-background" d="M36.0983333,22.7383333 C36.9280725,27.5056357 35.3832611,32.3782594 31.9583333,35.7966667 C31.0587397,36.6879974 30.0508686,37.4629382 28.9583333,38.1033333 C28.3033941,38.4890431 27.4704606,38.3827841 26.9333333,37.845 L23.6166667,34.53 C28.5053053,31.4496338 32.7452344,27.4440926 36.0983333,22.7383333 L36.0983333,22.7383333 Z" opacity="0.598539807"></path>
													</g>
												</g>
												</g>
											</g>
										</svg>
									</div>
									<h6>Develop</h6>
								</a-col>
							</a-row>
   							<a-checkbox-group class="d-none" v-model="checkedList" :options="checkboxOptions"/>
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
			
			<!-- Step 3 : Address -->
			<a-card v-else-if="step == 2" :bordered="false" class="header-solid">

				<h5 class="font-regular text-center">Are you living in a nice area?</h5>
				<p class="text-center">One thing I love about the later sunsets is the chance to go for a walk through the neighborhood woods before dinner</p>

				<a-form
					:form="form"
					class="mt-30"
					:hideRequiredMark="true"
				>
					<a-row :gutter="[24]">
						<a-col :span="24" :md="16">
							<a-form-item class="mb-10" label="Street Name" :colon="false">
								<a-input placeholder="eg. Street 111" />
							</a-form-item>
						</a-col>
						<a-col :span="24" :md="8">
							<a-form-item class="mb-10" label="Street No" :colon="false">
								<a-input placeholder="eg. Street 221" />
							</a-form-item>
						</a-col>
						<a-col :span="24" :md="14">
							<a-form-item class="mb-10" label="City" :colon="false">
								<a-input placeholder="eg. Tokyo" />
							</a-form-item>
						</a-col>
						<a-col :span="24" :md="10">
							<a-form-item class="mb-10" label="Country" :colon="false">
								<a-select default-value="Argentina" show-search>
									<a-select-option value="Argentina">Argentina</a-select-option>
									<a-select-option value="Albania">Albania</a-select-option>
									<a-select-option value="Algeria">Algeria</a-select-option>
									<a-select-option value="Andorra">Andorra</a-select-option>
									<a-select-option value="Angola">Angola</a-select-option>
									<a-select-option value="Brasil">Brasil</a-select-option>
								</a-select>
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
		<!-- / Wizard form cards -->

	</div>

</template>

<script>

	export default {
		components: {
		},
		data() {
			return {

				// Active step.
				step: 0,

				// Checkbox'es array of checked options.
      			checkedList: ['Design'],

				// Checkbox'es array of all options.
				checkboxOptions: ['Design', 'Code', 'Develop'],

				
				// Step's form object
				form: this.$form.createForm(this, { name: 'steps' }),

			}
		},
		methods: {

			// Move step by a distance.
			moveStep( distance ) {
				this.step += distance ;
			},

			// Toggle an item from the checkbox.
			toggleCheckbox(item) {
				const index = this.checkedList.indexOf( item );

				if( index > -1 )
				{
					this.checkedList.splice(index, 1);
				}
				else {
					this.checkedList.push( item ) ;
				}
			},
			
		},
	}

</script>

<style lang="scss">
</style>