module mod_material
  use mod_Ray
  use mod_vectors
  implicit none

  type, extends(Material) :: Lambertian
    real :: albedo(3)
  contains
    procedure, pass(self) :: scatter => lambertian_scatter
  end type Lambertian

  type, extends(Material) :: Metal 
    real :: albedo(3)
    real :: fuzz = 0.0
  contains
    procedure, pass(self) :: scatter => metal_scatter
  end type Metal
  
  type, extends(Material) :: Dielectric
    real :: refraction_index = 1.0
  contains
    procedure, pass(self) :: scatter => dielectric_scatter
  end type Dielectric
  
contains

 logical function lambertian_scatter(self, r_in, rec, attenuation, scattered)
    class(Lambertian), INTENT(INOUT) :: self
    type(Ray), intent(in) :: r_in
    type(HitRecord), intent(in) :: rec
    type(Ray), intent(out) :: scattered
    real, intent(out) :: attenuation(3)
    real :: scatter_direction(3)

    scatter_direction = rec%normal + random_unit_vector()

    if (near_zero(scatter_direction)) scatter_direction = rec%normal
    
    
    scattered = Ray(rec%p, scatter_direction)
    attenuation = self%albedo
    lambertian_scatter = .true. 
  end function lambertian_scatter

 logical function metal_scatter(self, r_in, rec, attenuation, scattered)
    class(Metal), INTENT(INOUT) :: self
    type(Ray), intent(in) :: r_in
    type(HitRecord), intent(in) :: rec
    type(Ray), intent(out) :: scattered
    real, intent(out) :: attenuation(3)
    real :: reflected(3)

    reflected = reflect(unit_vector(r_in%direction), rec%normal)
    scattered = Ray(rec%p, reflected + self%fuzz * random_in_unit_sphere())
    attenuation = self%albedo
    
    metal_scatter = (scattered%direction .dot. rec%normal) > 0  
  end function metal_scatter
 
  logical function dielectric_scatter(self, r_in, rec, attenuation, scattered)
    class(Dielectric), INTENT(INOUT) :: self
    type(Ray), intent(in) :: r_in
    type(HitRecord), intent(in) :: rec
    type(Ray), intent(out) :: scattered
    real, intent(out) :: attenuation(3)
    real :: refraction_ratio, unit_direction(3), direction(3)
    real :: cos_theta, sin_theta
    logical :: cannot_refract

    attenuation = [1.0, 1.0, 1.0]
    if (rec%front_face) then
      refraction_ratio = 1.0/self%refraction_index
    else 
      refraction_ratio = self%refraction_index
    end if

    unit_direction = unit_vector(r_in%direction)
    cos_theta = min(unit_direction .dot. rec%normal, 1.0)
    sin_theta = sqrt(1.0 - cos_theta * cos_theta)
    cannot_refract = refraction_ratio * sin_theta > 1.0

    if (cannot_refract) then
      direction = reflect(unit_direction, rec%normal)
    else
      direction = refract(unit_direction, rec%normal, refraction_ratio)
    end if


    scattered = Ray(rec%p, direction)
    dielectric_scatter = .true.
  end function dielectric_scatter
end module mod_material