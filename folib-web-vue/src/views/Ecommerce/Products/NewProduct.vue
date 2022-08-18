<!-- 
	This is the new product page, it uses the dashboard layout in: 
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

		<!-- Steps and forms -->
		<div class="mx-auto m-50" style="max-width: 800px;">

			<!-- Steps -->
			<div class="mb-50" style="max-width: 800px;">

				<a-steps progress-dot v-model="step">
					<a-step title="1. Product Info" />
					<a-step title="2. Media" />
					<a-step title="3. Socials" />
					<a-step title="4. Pricing" />
				</a-steps>

			</div>
			<!-- / Steps -->

			<!-- Steps tabs and forms -->
			<div class="mb-50">

				<!-- Step 1 : Product Information -->
				<a-card v-if="step == 0" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
					
					<template #title>
						<h5 class="mb-0">Product Information</h5>
					</template>

					<!-- Product Information form -->
					<a-form
						:form="form"
						@submit="handleSubmit"
						:hideRequiredMark="true"
					>
						<a-row :gutter="[24]">
							<a-col :span="12">
								<a-form-item class="mb-10" label="Name" :colon="false">
									<a-input placeholder="eg. Off-White" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Weight" :colon="false">
									<a-input placeholder="eg. 42" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Description" :colon="false">
									<quill-editor v-model="quillContent" :options="quillOptions" style="height: 85px;" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Category" :colon="false">
									<a-select default-value="Clothing">
										<a-select-option value="Clothing">Clothing</a-select-option>
										<a-select-option value="Electronics">Electronics</a-select-option>
										<a-select-option value="Furniture">Furniture</a-select-option>
										<a-select-option value="RealEstate">Real Estate</a-select-option>
										<a-select-option value="Others">Others</a-select-option>
									</a-select>
								</a-form-item>
								<a-form-item class="mb-10" label="Sizes" :colon="false">
									<a-select default-value="Medium">
										<a-select-option value="XS">Extra Small</a-select-option>
										<a-select-option value="Small">Small</a-select-option>
										<a-select-option value="Medium">Medium</a-select-option>
										<a-select-option value="Large">Large</a-select-option>
										<a-select-option value="XL">Extra Large</a-select-option>
									</a-select>
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
					<!-- / Product Information form -->

				</a-card>

				<!-- Step 2 : Media -->
				<a-card v-else-if="step == 1" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
					
					<template #title>
						<h5 class="mb-0">Media</h5>
					</template>

					<!-- Media form -->
					<a-form
						:form="form"
						@submit="handleSubmit"
						:hideRequiredMark="true"
					>
						<a-row :gutter="[24]">
							<a-col :span="24">
								<a-form-item class="mb-10" label="Product images" :colon="false">
									<a-upload
										name="avatar"
										list-type="picture-card"
										class="projects-uploader"
										:show-upload-list="false"
										style="height: 150px;"
									>
										<img v-if="false" src="" alt="avatar" />
										<div v-else>
											<div class="ant-upload-text font-regular text-muted">
												Drop files here to upload
											</div>
										</div>
									</a-upload>
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
					<!-- / Media form -->

				</a-card>

				<!-- Step 3 : Socials -->
				<a-card v-else-if="step == 2" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
					
					<template #title>
						<h5 class="mb-0">Socials</h5>
					</template>

					<!-- Socials form -->
					<a-form
						:form="form"
						@submit="handleSubmit"
						:hideRequiredMark="true"
					>
						<a-row :gutter="[24]">
							<a-col :span="24">
								<a-form-item class="mb-10" label="Shoppify Handle" :colon="false">
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
					<!-- / Socials form -->

				</a-card>

				<!-- Step 4 : Pricing -->
				<a-card v-else-if="step == 3" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }" :headStyle="{paddingBottom: '0' }">
					
					<template #title>
						<h5 class="mb-0">Pricing</h5>
					</template>
					
					<!-- Pricing form -->
					<a-form
						:form="form"
						@submit="handleSubmit"
						:hideRequiredMark="true"
					>
						<a-row :gutter="[24]">
							<a-col :span="24" :md="6">
								<a-form-item class="mb-10" label="Price" :colon="false">
									<a-input placeholder="99.00" />
								</a-form-item>
							</a-col>
							<a-col :span="24" :md="8">
								<a-form-item class="mb-10" label="Currency" :colon="false">
									<a-select default-value="USD">
										<a-select-option value="USD">USD</a-select-option>
										<a-select-option value="EUR">EUR</a-select-option>
										<a-select-option value="GBP">GBP</a-select-option>
										<a-select-option value="INR">INR</a-select-option>
										<a-select-option value="CNY">CNY</a-select-option>
										<a-select-option value="BTC">BTC</a-select-option>
									</a-select>
								</a-form-item>
							</a-col>
							<a-col :span="24" :md="10">
								<a-form-item class="mb-10" label="SKU" :colon="false">
									<a-input placeholder="71283476591" />
								</a-form-item>
							</a-col>
							<a-col :span="24">
								<a-form-item class="tags-field mb-10" label="Tags" :colon="false">
									<a-select mode="multiple" v-model="selectedTags" style="width: 100%" placeholder="Tags">
										<a-select-option v-for="tag in tags" :key="tag" :value="tag" selected>
											{{ tag }}
										</a-select-option>
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
					<!-- / Pricing form -->

				</a-card>

			</div>
			<!-- / Steps tabs and forms -->

		</div>
		<!-- / Steps and forms -->

	</div>

</template>

<script>

	import 'quill/dist/quill.core.css' ;
	import 'quill/dist/quill.snow.css' ;
	import { quillEditor } from 'vue-quill-editor'

	// Available tags.
	const tags = ['Vuejs', 'Angular', 'React'] ;

	export default {
		components: {
			quillEditor,
		},
		data() {
			return {

				// Active button for the "User Profile" card's radio button group.
				profileHeaderBtns: 'overview',
				
				// Current active step.
				step: 0,

				// Available tags.
				tags,

				// Selected tags.
				selectedTags: tags,

				// Form object.
				form: this.$form.createForm(this, { name: 'newProduct' }),

				// Quill editor html content.
				quillContent: '<p>Some initial <strong>bold</strong> text</p>',

				// Quill editor options.
				quillOptions: {
					modules: {
						toolbar: [
							[{ 'header': [1, 2, 3, false] }],
							['bold', 'italic', 'underline'],
							[{ 'list': 'ordered' }, { 'list': 'bullet' }, 'link'],
							['clean'],
						]
					},
				}
				
			}
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

			// Move step by a distance.
			moveStep( distance ) {
				this.step += distance ;
			},
			
		},
	}

</script>

<style lang="scss">
</style>