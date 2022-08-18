<!-- 
	This is the Product edit page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<!-- Product edit form -->
		<a-form
			:form="form"
			:hideRequiredMark="true"
		>

			<!-- Form header -->
			<a-row type="flex" :gutter="24" align="middle">
				<a-col :span="24" :lg="12" class="mb-24">
					<h4>Make the changes below</h4>
					<p>We’re constantly trying to express ourselves and actualize our dreams. If you have the opportunity to play.</p>
				</a-col>
				<a-col :span="24" :lg="12" class="mb-24 text-right">
					<a-button type="primary">SAVE</a-button>
				</a-col>
			</a-row>
			<!-- / Form header -->

			<a-row type="flex" :gutter="24">

				<!-- Product Image col -->
				<a-col :span="24" :lg="8" class="mb-24">
					
					<!-- Product Image card -->
					<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0 }">
						<template #title>
							<h5 class="mb-0">Product Image</h5>
						</template>
							<img class="product-img shadow-3 img-contain" src="https://images.unsplash.com/photo-1555041469-a586c61ea9bc?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&amp;ixlib=rb-1.2.1&amp;auto=format&amp;fit=crop&amp;w=1950&amp;q=80" alt="product_image">

							<div class="mt-20">
								<a-button type="primary" class="px-25">EDIT</a-button>
								<a-button class="ml-10 px-25">REMOVE</a-button>
							</div>
					</a-card>
					<!-- / Product Image card -->

				</a-col>
				<!-- / Product Image col -->
				
				<!-- Product Information col -->
				<a-col :span="24" :lg="16" class="mb-24">
					
					<!-- Product Information card -->
					<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: '40px', }">
						<template #title>
							<h5 class="mb-0">Product Information</h5>
						</template>
						<a-row :gutter="[24]">
							<a-col :span="12">
								<a-form-item class="mb-10" label="Name" :colon="false">
									<a-input placeholder="eg. Minimal Bar Stool" defaultValue="Minimal Bar Stool" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Weight" :colon="false">
									<a-input placeholder="eg. 42" defaultValue="2" />
								</a-form-item>
							</a-col>
							<a-col :span="6">
								<a-form-item class="mb-10" label="Collection" :colon="false">
									<a-input placeholder="eg. Summer" defaultValue="Summer" />
								</a-form-item>
							</a-col>
							<a-col :span="6">
								<a-form-item class="mb-10" label="Price" :colon="false">
									<a-input placeholder="eg. $90" defaultValue="$90" />
								</a-form-item>
							</a-col>
							<a-col :span="6">
								<a-form-item class="mb-10" label="Quantity" :colon="false">
									<a-input placeholder="eg. 2" defaultValue="2" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Description" :colon="false">
									<quill-editor v-model="quillContent" :options="quillOptions" style="height: 85px;" />
								</a-form-item>
							</a-col>
							<a-col :span="12">
								<a-form-item class="mb-10" label="Category" :colon="false">
									<a-select default-value="Furniture">
										<a-select-option value="Clothing">Clothing</a-select-option>
										<a-select-option value="Electronics">Electronics</a-select-option>
										<a-select-option value="Furniture">Furniture</a-select-option>
										<a-select-option value="RealEstate">Real Estate</a-select-option>
										<a-select-option value="Others">Others</a-select-option>
									</a-select>
								</a-form-item>
								<a-form-item class="mb-10" label="Color" :colon="false">
									<a-select default-value="Green">
										<a-select-option value="Black">Black</a-select-option>
										<a-select-option value="Blue">Blue</a-select-option>
										<a-select-option value="Green">Green</a-select-option>
										<a-select-option value="Orange">Orange</a-select-option>
										<a-select-option value="White">White</a-select-option>
									</a-select>
								</a-form-item>
							</a-col>
						</a-row>
					</a-card>
					<!-- / Product Information card -->

				</a-col>
				<!-- / Product Information col -->

			</a-row>

			<a-row type="flex" :gutter="24">

				<!-- Socials col -->
				<a-col :span="24" :lg="8" class="mb-24">
					
					<!-- Socials card -->
					<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0 }">
						<template #title>
							<h5 class="mb-0">Socials</h5>
						</template>
						<a-form-item class="mb-10" label="Shoppify Handle" :colon="false">
							<a-input placeholder="@Soft" />
						</a-form-item>
						<a-form-item class="mb-10" label="Facebook Account" :colon="false">
							<a-input placeholder="http://..." />
						</a-form-item>
						<a-form-item class="mb-10" label="Instagram Account" :colon="false">
							<a-input placeholder="http://..." />
						</a-form-item>
					</a-card>
					<!-- / Socials card -->

				</a-col>
				<!-- / Socials col -->

				<!-- Pricing col -->
				<a-col :span="24" :lg="16" class="mb-24">
					
					<!-- Pricing card -->
					<a-card :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }">
						<template #title>
							<h5 class="mb-0">Pricing</h5>
						</template>
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
									<a-select mode="multiple" v-model="selectedTags" style="width: 100%" placeholder="Tags Mode">
										<a-select-option v-for="tag in tags" :key="tag" :value="tag" selected>
											{{ tag }}
										</a-select-option>
									</a-select>
								</a-form-item>
							</a-col>
						</a-row>
					</a-card>
					<!-- / Pricing card -->

				</a-col>
				<!-- / Pricing col -->

			</a-row>

		</a-form>
		<!-- / Product edit form -->

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

				// Current active step.
				step: 0,

				// Available tags.
				tags,

				// Selected tags.
				selectedTags: tags,

				// Form object.
				form: this.$form.createForm(this, { name: 'editProduct' }),

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
	}

</script>

<style lang="scss" scoped>
	.product-img {
	    border-radius: 12px;
	}
</style>