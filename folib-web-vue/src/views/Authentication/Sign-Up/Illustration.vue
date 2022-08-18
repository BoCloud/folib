<!-- 
	This is the illustration sign up page, it uses the dashboard layout in: 
	"./layouts/Default.vue" .
 -->

<template>
	<div class="sign-in">
		
		<a-row type="flex" :gutter="[24,24]" justify="space-around" align="middle" class="row-main">

			<!-- Sign Up Form Column -->
			<a-col :span="24" :md="{ span: 14, offset: 2 }" :lg="10" :xl="6" class="col-form mr-auto">
				<h4 class="mb-15">Sign Up</h4>
				<p class="text-muted">Enter your email and password to register</p>

				<!-- Sign Up Form -->
				<a-form
					id="components-form-demo-normal-login"
					:form="form"
					class="login-form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-form-item class="mb-10" label="Name" :colon="false">
						<a-input 
						v-decorator="[
						'name',
						{ rules: [{ required: true, message: 'Please input your name!' }] },
						]" placeholder="Name" />
					</a-form-item>
					<a-form-item class="mb-10" label="Email" :colon="false">
						<a-input 
						v-decorator="[
						'email',
						{ rules: [{ required: true, message: 'Please input your email!' }] },
						]" placeholder="Email" />
					</a-form-item>
					<a-form-item class="mb-5" label="Password" :colon="false">
						<a-input
						v-decorator="[
						'password',
						{ rules: [{ required: true, message: 'Please input your password!' }] },
						]" type="password" placeholder="Password" />
					</a-form-item>
					<a-form-item class="mb-10">
						<a-checkbox
							v-decorator="[
							'remember',
							{
								valuePropName: 'checked',
								initialValue: true,
							},
							]"
						>
							I agree to the <a href="#" class="font-bold text-dark">Terms and Conditions</a>
						</a-checkbox>
					</a-form-item>
					<a-form-item>
						<a-button type="primary" block html-type="submit" class="login-form-button">
							SIGN UP
						</a-button>
					</a-form-item>
				</a-form>
				<!-- / Sign Up Form -->

			<p class="font-semibold text-muted text-center">Already have an account? <router-link to="/sign-in" class="font-bold text-dark">Sign Up</router-link></p>
			</a-col>
			<!-- / Sign Up Form Column -->

			<!-- Sign Up Image Column -->
			<a-col :span="24" :md="12" :lg="12" :xl="12" class="col-img">
				<div>
					<div class="img">
						<img src="images/info-rocket-ill.png" alt="rocket">
					</div>
					<h4 class="text-white">Your journey starts here</h4>
					<p class="text-white">Just as it takes a company to sustain a product, it takes a community to sustain a protocol.</p>
				</div>
			</a-col>
			<!-- / Sign Up Image Column -->

		</a-row>
		
	</div>
</template>

<script>

	export default ({
		data() {
			return {
				// Sign up form object.
				form: this.$form.createForm(this, { name: 'signup_illustration' }),
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
		},
	})

</script>

<style lang="scss">
</style>