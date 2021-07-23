module mod_types
  use mod_vectors
  use iso_fortran_env, only: stdout => output_unit, stderr => error_unit

  implicit none


  type :: Ray
    real, DIMENSION(3) :: origin, direction
  contains
    procedure, pass(self) :: at => get_point_at
  end type Ray

  type Hittable 
  contains
    procedure, public, pass(self) :: hit => hit_hittable
  end type Hittable

  type :: HitRecord
    real ::t, p(3) = [0, 0, 0], normal(3) = [0,0,0]
    logical :: front_face = .true.
  contains
    procedure, pass(self) :: set_face_normal
  end type HitRecord

  type, extends(Hittable) :: Sphere
    real :: center(3) = [0.0, 0.0, 0.0]
    real :: radius = 1.0
  contains
    procedure, public, pass(self) :: hit => hit_sphere
  end type Sphere
  
  type, extends(Hittable) :: HittableList
    type(Sphere), ALLOCATABLE :: hit_list(:)
  contains
    procedure, public, pass(self) :: hit => hit_hittable_list
  end type HittableList

contains
  pure function get_point_at(self, t) result(at)
    class(Ray), INTENT(IN) :: self
    real, INTENT(IN) :: t
    real :: at(3)

    at = self%origin + self%direction * t
  end function get_point_at

  subroutine set_face_normal(self, r, outward_normal)
    class(HitRecord), INTENT(INOUT) :: self
    class(Ray), INTENT(IN) :: r
    real, INTENT(IN) :: outward_normal(3)

    self%front_face = (r%direction .dot. outward_normal) < 0
    if (self%front_face) then
      self%normal = outward_normal
    else 
      self%normal = -outward_normal
    end if
  end subroutine set_face_normal

  logical function hit_hittable(self, r, t_min, t_max, rec)
    class(Ray), INTENT(IN) :: r
    class(Hittable), INTENT(IN) ::self
    type(HitRecord), INTENT(OUT) :: rec
    real, INTENT(IN) :: t_min, t_max
  
    rec = HitRecord(0)
    hit_hittable = .false.
  end function hit_hittable

  logical function hit_hittable_list(self, r, t_min, t_max, rec)
    class(Ray), INTENT(IN) :: r
    class(HittableList), INTENT(IN) ::self
    type(HitRecord), INTENT(OUT) :: rec
    type(Sphere), ALLOCATABLE :: obj
    real, INTENT(IN) :: t_min, t_max


    type(HitRecord) :: tmp_rec
    real :: closest 
    logical :: hit_anything
    integer :: i

    hit_anything = .false.
    closest = t_max

    do i=1, size(self%hit_list)
      obj = self%hit_list(i)
      if (hit_sphere(obj, r, t_min, closest, tmp_rec)) then
        hit_anything = .true.
        closest = tmp_rec%t
        rec = tmp_rec
      end if
    end do
  
    hit_hittable_list = hit_anything
  end function hit_hittable_list

  logical function hit_sphere(self, r, t_min, t_max, rec)
    class(Ray), INTENT(IN) :: r
    class(Sphere), INTENT(IN) ::self
    type(HitRecord), INTENT(OUT) :: rec
    real, INTENT(IN) :: t_min, t_max
    real :: oc(3),  outward_normal(3)
    real :: a, half_b, c, discriminant, sqrt_d, root
    
    oc = r%origin - self%center
    a = length_squared(r%direction) ! r%direction .dot. r%direction
    half_b = (oc .dot. r%direction)
    c  = length_squared(oc) - self%radius * self%radius
    discriminant = half_b * half_b - a*c

    if (discriminant < 0) then
      hit_sphere = .false.
      RETURN
    end if

    sqrt_d = sqrt(discriminant)

    ! Find nearest hit
    root = (-half_b - sqrt_d) / a
    if (root < t_min .or. t_max < root) then
      root = (-half_b + sqrt_d) / a
      if (root < t_min .or. t_max < root) then
        hit_sphere = .false.
        RETURN
      end if
    end if

    rec = HitRecord(t=root, p=r%at(root))
    outward_normal = (rec%p - self%center) / self%radius 
    call rec % set_face_normal(r, outward_normal)
    hit_sphere = .true.
  end function 
  
end module mod_types